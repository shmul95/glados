module Rune.IR.Generator.Expression.Call (genCall) where

import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (registerCall, newTemp)
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
-- private
--

mangleName :: String -> [([IRInstruction], IROperand, IRType)] -> String
mangleName base ((_, _, IRStruct s) : _) = s ++ "_" ++ base
mangleName base ((_, _, IRPtr (IRStruct s)) : _) = s ++ "_" ++ base
mangleName base _ = base

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, op, _) = (i, op)

