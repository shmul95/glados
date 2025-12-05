module Rune.IR.Generator.Expression.Call (genCall, genShowCall) where

import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (genFormatString, newTemp, registerCall)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  argsData <- mapM genExpr args

  let mangled = mangleName funcName argsData
      (instrs, ops) = unzip $ map prepareArg argsData
      allInstrs = concat instrs

      -- TODO: improve return type inference:
      --  currently only handles struct and pointer-to-struct arguments
      --  otherwise defaults to IRI32
      --
      --  i will implement robust detection for all function signatures later
      --  when the semantic analysis is more complete
      retType = case argsData of
        ((_, _, IRStruct s) : _) -> IRStruct s
        ((_, _, IRPtr (IRStruct s)) : _) -> IRStruct s
        _ -> IRI32

  registerCall mangled
  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp mangled ops (Just retType)

  return (allInstrs ++ [callInstr], IRTemp retTemp retType, retType)

--
-- show calls
--
-- show is a built-in function that prints the value of its argument to stdout
-- def show(value: any) -> null
--

genShowCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  let funcName = getShowFunc op typ
      (prep, finalOp) = prepareAddr op typ

  registerCall funcName
  (fmtInstrs, callArgs) <- genShowFmtCall op typ finalOp

  let callInstr = IRCALL "" funcName callArgs Nothing
  return (instrs ++ prep ++ fmtInstrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)

--
-- private
--

-- | as show is a built-in "printf"-like function
-- def show(value: any) -> null
-- we need to format the arguments accordingly to the input
genShowFmtCall :: IROperand -> IRType -> IROperand -> IRGen ([IRInstruction], [IROperand])
genShowFmtCall originalOp typ finalOp = case getFormatSpecifier originalOp typ of
  Just fmt -> do
    (i, f) <- genFormatString fmt
    return (i, [f, finalOp])
  Nothing -> return ([], [finalOp])

getShowFunc :: IROperand -> IRType -> String
getShowFunc _ (IRStruct s) = "show_" ++ s
getShowFunc _ (IRPtr (IRStruct s)) = "show_" ++ s
getShowFunc _ _ = "printf"

getFormatSpecifier :: IROperand -> IRType -> Maybe String
getFormatSpecifier _ IRI8 = Just "%hhd"
getFormatSpecifier _ IRI16 = Just "%hd"
getFormatSpecifier _ IRI32 = Just "%d"
getFormatSpecifier _ IRI64 = Just "%ld"
getFormatSpecifier _ IRU8 = Just "%hhu"
getFormatSpecifier _ IRU16 = Just "%hu"
getFormatSpecifier _ IRU32 = Just "%u"
getFormatSpecifier _ IRU64 = Just "%lu"
getFormatSpecifier _ IRChar = Just "%c"
getFormatSpecifier _ IRF32 = Just "%f"
getFormatSpecifier _ IRF64 = Just "%lf"
getFormatSpecifier _ IRBool = Just "%d"
getFormatSpecifier _ IRNull = Just "(null)"
getFormatSpecifier _ (IRPtr IRChar) = Just "%s"
getFormatSpecifier _ _ = Nothing

mangleName :: String -> [([IRInstruction], IROperand, IRType)] -> String
mangleName base ((_, _, IRStruct s) : _) = s ++ "_" ++ base
mangleName base ((_, _, IRPtr (IRStruct s)) : _) = s ++ "_" ++ base
mangleName base _ = base

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, op, _) = (i, op)

prepareAddr :: IROperand -> IRType -> ([IRInstruction], IROperand)
prepareAddr (IRTemp n _) (IRStruct t) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRStruct t))],
    IRTemp ("addr_" ++ n) (IRPtr (IRStruct t))
  )
prepareAddr (IRTemp n _) (IRPtr (IRStruct t)) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRPtr (IRStruct t)))],
    IRTemp ("addr_" ++ n) (IRPtr (IRPtr (IRStruct t)))
  )
prepareAddr op _ = ([], op)
