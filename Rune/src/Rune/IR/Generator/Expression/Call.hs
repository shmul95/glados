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

genShowCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  let funcName = getShowFunc typ
      (prep, finalOp) = prepareAddr op typ

  registerCall funcName
  (fmtInstrs, callArgs) <- genShowFmtCall typ finalOp

  let callInstr = IRCALL "" funcName callArgs Nothing
  return (instrs ++ prep ++ fmtInstrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)

-- | as show is a built-in "printf"-like function
-- def show(value: any) -> null
-- we need to format the arguments accordingly to the input
--
-- TODO: add mapping for other types
genShowFmtCall :: IRType -> IROperand -> IRGen ([IRInstruction], [IROperand])
genShowFmtCall IRF32 finalOp = do
  (i, f) <- genFormatString "%f"
  return (i, [f, finalOp])
genShowFmtCall IRF64 finalOp = do
  (i, f) <- genFormatString "%lf"
  return (i, [f, finalOp])
genShowFmtCall IRI32 finalOp = do
  (i, f) <- genFormatString "%d"
  return (i, [f, finalOp])
genShowFmtCall IRI64 finalOp = do
  (i, f) <- genFormatString "%ld"
  return (i, [f, finalOp])
genShowFmtCall (IRPtr IRU8) finalOp =
  return ([], [finalOp])
genShowFmtCall _ finalOp =
  return ([], [finalOp])

--
-- private
--

mangleName :: String -> [([IRInstruction], IROperand, IRType)] -> String
mangleName base args = case args of
  ((_, _, IRStruct s) : _) -> s ++ "_" ++ base
  ((_, _, IRPtr (IRStruct s)) : _) -> s ++ "_" ++ base
  _ -> base

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, op, IRStruct _) = case op of
  IRTemp n t -> (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
  _ -> (i, op)
prepareArg (i, op, _) = (i, op)

getShowFunc :: IRType -> String
getShowFunc (IRStruct s) = "show_" ++ s
getShowFunc (IRPtr (IRStruct s)) = "show_" ++ s
getShowFunc IRU8 = "putchar"
getShowFunc _ = "printf"

prepareAddr :: IROperand -> IRType -> ([IRInstruction], IROperand)
prepareAddr op (IRStruct t) = case op of
  IRTemp n _ -> ([IRADDR ("addr_" ++ n) n (IRPtr (IRStruct t))], IRTemp ("addr_" ++ n) (IRPtr (IRStruct t)))
  _ -> ([], op)
prepareAddr op _ = ([], op)
