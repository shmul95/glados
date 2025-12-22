module Rune.IR.Generator.Expression.Array
  ( genLitArray,
    genIndex,
    genIndexAssign
  )
where

import Control.Monad.Except (throwError)
import Rune.AST.Nodes (Expression (..))
import Rune.IR.IRHelpers (newTemp, registerVar)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- types
--

type ArrayGen = IRGen ([IRInstruction], IROperand, IRType)
type ArrayGenFunc = Expression -> ArrayGen

--
-- public
--

-- | generate an array literal
-- arr0: *[char x 4] = ALLOC_ARRAY char ['a', 'b', 'c', 'd']
-- arr1: *[i32 x 5] = ALLOC_ARRAY i32 [1, 2, 3, 4, 5]
genLitArray :: (ArrayGenFunc) -> [Expression] -> ArrayGen
genLitArray _ [] = throwError "genLitArray: empty array not supported"
genLitArray genExpr exprs = do
  (instrs, ops, types) <- unzip3 <$> mapM genExpr exprs
  let allInstrs = concat instrs
      elemType = case types of
                   (t:_) -> t
                   [] -> error "genLitArray: impossible empty types"
      arrType = IRArray elemType (length ops)
      ptrType = IRPtr arrType
  
  tempName <- newTemp "arr" ptrType
  let allocInstr = IRALLOC_ARRAY tempName elemType ops
      result = IRTemp tempName ptrType
  
  registerVar tempName result ptrType
  return (allInstrs <> [allocInstr], result, ptrType)

-- | generate array indexing
-- elem1: char = GET_ELEM arr0[0]
genIndex :: (ArrayGenFunc) -> Expression -> ArrayGenFunc
genIndex genExpr target idx = do
  (targetInstrs, targetOp, targetType) <- genExpr target
  (idxInstrs, idxOp, _) <- genExpr idx
  
  elemType <- case targetType of
    IRPtr (IRArray t _) -> return t
    _ -> throwError $ "genIndex: expected array type, got " <> show targetType
  
  tempName <- newTemp "elem" elemType
  let getInstr = IRGET_ELEM tempName targetOp idxOp elemType
      result = IRTemp tempName elemType
  
  return (targetInstrs <> idxInstrs <> [getInstr], result, elemType)

-- | generate array index assignment
-- SET_ELEM arr0[1] = 'z'
genIndexAssign :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> Expression -> Expression -> Expression -> IRGen [IRInstruction]
genIndexAssign genExpr target idx value = do
  (targetInstrs, targetOp, _) <- genExpr target
  (idxInstrs, idxOp, _) <- genExpr idx
  (valInstrs, valOp, _) <- genExpr value
  
  let setInstr = IRSET_ELEM targetOp idxOp valOp
  
  return (targetInstrs <> idxInstrs <> valInstrs <> [setInstr])
