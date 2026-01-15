-- file: src/Rune/IR/Generator/Expression/Unary.hs

{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Unary
  ( genUnary,
    genUnaryExpr,
    genUnaryNegate,
    genUnaryNot,
    genUnaryBitNot,
    genUnaryPrefixInc,
    genUnaryPrefixDec,
    genUnaryPostfixInc,
    genUnaryPostfixDec,
    genUnaryPropagate,
    genUnaryDeref,
    genUnaryReference
  )
where
#else
module Rune.IR.Generator.Expression.Unary
  ( genUnary,
  )
where
#endif

import Rune.AST.Nodes (Expression, UnaryOp (..))
import Rune.IR.IRHelpers (newTemp, nextLabelIndex, makeLabel)
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
genUnaryExpr Not = genUnaryNot
genUnaryExpr BitNot = genUnaryBitNot
genUnaryExpr PrefixInc = genUnaryPrefixInc
genUnaryExpr PrefixDec = genUnaryPrefixDec
genUnaryExpr PostfixInc = genUnaryPostfixInc
genUnaryExpr PostfixDec = genUnaryPostfixDec
genUnaryExpr PropagateError = genUnaryPropagate
genUnaryExpr Deref = genUnaryDeref
genUnaryExpr Reference = genUnaryReference

genUnaryNegate :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryNegate instrs operand typ = do
  t <- newTemp "t" typ
  let i = IRSUB_OP t (IRConstInt 0) operand typ
  return (instrs ++ [i], IRTemp t typ, typ)

genUnaryNot :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryNot instrs operand _ = do
  t <- newTemp "t" IRBool
  let i = IRCMP_EQ t operand (IRConstBool False)
  return (instrs ++ [i], IRTemp t IRBool, IRBool)

genUnaryBitNot :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryBitNot instrs operand typ = do
  t <- newTemp "t" typ
  let i = IRBNOT_OP t operand typ
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
genUnaryPropagate instrs operand typ = do
  idx <- nextLabelIndex
  let okLabel = makeLabel "propagate_ok" idx
  
  let propagation = 
        [ IRJUMP_TRUE operand okLabel
        , IRRET Nothing
        , IRLABEL okLabel
        ]

  return (instrs ++ propagation, operand, typ)

genUnaryDeref :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryDeref instrs operand (IRPtr innerType) = do
  t <- newTemp "t" innerType
  let i = IRDEREF t operand innerType
  return (instrs ++ [i], IRTemp t innerType, innerType)
genUnaryDeref _ _ typ = 
  error $ "Cannot dereference non-pointer type: " ++ show typ

genUnaryReference :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genUnaryReference instrs (IRTemp name _) typ = do
  t <- newTemp "t" (IRPtr typ)
  let i = IRADDR t name (IRPtr typ)
  return (instrs ++ [i], IRTemp t (IRPtr typ), IRPtr typ)
genUnaryReference instrs (IRParam name _) typ = do
  t <- newTemp "t" (IRPtr typ)
  let i = IRADDR t name (IRPtr typ)
  return (instrs ++ [i], IRTemp t (IRPtr typ), IRPtr typ)
genUnaryReference _ operand typ = 
  error $ "Cannot take address of non-lvalue: " ++ show operand ++ " of type " ++ show typ
