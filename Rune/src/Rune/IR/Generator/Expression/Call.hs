module Rune.IR.Generator.Expression.Call (genCall, genShowCall) where

import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (newTemp)
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

  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp mangled ops (Just retType)

  return (allInstrs ++ [callInstr], IRTemp retTemp retType, retType)

genShowCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  let funcName = getShowFunc typ
      (prep, finalOp) = prepareAddr op typ

  let callInstr = IRCALL "" funcName [finalOp] Nothing
  return (instrs ++ prep ++ [callInstr], IRTemp "t_void" IRVoid, IRVoid)

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
getShowFunc (IRPtr IRU8) = "puts"
getShowFunc IRU8 = "putchar"
getShowFunc _ = "printf"

prepareAddr :: IROperand -> IRType -> ([IRInstruction], IROperand)
prepareAddr op (IRStruct t) = case op of
  IRTemp n _ -> ([IRADDR ("addr_" ++ n) n (IRPtr (IRStruct t))], IRTemp ("addr_" ++ n) (IRPtr (IRStruct t)))
  _ -> ([], op)
prepareAddr op _ = ([], op)
