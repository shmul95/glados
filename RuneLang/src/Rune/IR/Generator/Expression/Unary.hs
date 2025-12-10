module Rune.IR.Generator.Expression.Unary
  ( genUnary,
  )
where

import Rune.AST.Nodes (Expression, UnaryOp (..))
import Rune.IR.IRHelpers (newTemp)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genUnary :: GenExprCallback -> UnaryOp -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genUnary genExpr op expr = do
  (instrs, operand, typ) <- genExpr expr
  genUnaryExpr op instrs operand typ

--
-- private
--

genUnaryExpr :: UnaryOp -> [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryExpr Negate = genUnaryNegate
genUnaryExpr PrefixInc = genUnaryPrefixInc
genUnaryExpr PrefixDec = genUnaryPrefixDec
genUnaryExpr PostfixInc = genUnaryPostfixInc
genUnaryExpr PostfixDec = genUnaryPostfixDec
genUnaryExpr PropagateError = genUnaryPropagate

genUnaryNegate :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryNegate instrs operand typ = do
  t <- newTemp "t" typ
  let i = IRSUB_OP t (IRConstInt 0) operand typ
  return (instrs ++ [i], IRTemp t typ, typ)

genUnaryPrefixInc :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryPrefixInc instrs operand typ =
  return (instrs ++ [IRINC operand], operand, typ)

genUnaryPrefixDec :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryPrefixDec instrs operand typ =
  return (instrs ++ [IRDEC operand], operand, typ)

genUnaryPostfixInc :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryPostfixInc instrs operand typ = do
  t <- newTemp "t" typ
  return (instrs ++ [IRASSIGN t operand typ, IRINC operand], IRTemp t typ, typ)

genUnaryPostfixDec :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryPostfixDec instrs operand typ = do
  t <- newTemp "t" typ
  return (instrs ++ [IRASSIGN t operand typ, IRDEC operand], IRTemp t typ, typ)

genUnaryPropagate :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryPropagate instrs operand typ = return (instrs, operand, typ)
