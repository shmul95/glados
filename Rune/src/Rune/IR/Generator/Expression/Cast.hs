module Rune.IR.Generator.Expression.Cast (genCast) where

import Rune.AST.Nodes (Expression, Type)
import Rune.IR.IRHelpers (astTypeToIRType, newTemp)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- public
--

genCast :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genCast genExpr expr targetType = do
  (instrs, op, fromType) <- genExpr expr
  let toType = astTypeToIRType targetType
  temp <- newTemp "t" toType
  let castInstr = IRCAST temp op fromType toType
  return (instrs ++ [castInstr], IRTemp temp toType, toType)
