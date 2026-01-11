{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.GenExpression
  ( genExpression,
    genVar
  )
where
#else
module Rune.IR.Generator.GenExpression (genExpression) where
#endif

import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import Rune.AST.Nodes (Expression (..), Type (..))
import Rune.IR.Generator.Expression.Binary (genBinary)
import Rune.IR.Generator.Expression.Call (genCall)
import Rune.IR.Generator.Expression.Call.Error (genErrorCall)
import Rune.IR.Generator.Expression.Call.Show (genShowCall)
import Rune.IR.Generator.Expression.Literals
import Rune.IR.Generator.Expression.Struct (genAccess, genStructInit)
import Rune.IR.Generator.Expression.Unary (genUnary)
import Rune.IR.Generator.Expression.Array (genLitArray, genIndex)
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IROperand (..), IRType (..))
import Rune.IR.IRHelpers (astTypeToIRType, newTemp, sizeOfIRType)

--
-- public
--

genExpression :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExpression (ExprLitInt _ n) = genLitInt n
genExpression (ExprLitFloat _ f) = genLitFloat f
genExpression (ExprLitChar _ c) = genLitChar c
genExpression (ExprLitBool _ b) = genLitBool b
genExpression (ExprLitNull _) = genLitNull
genExpression (ExprLitString _ s) = genLitString s
genExpression (ExprVar _ name) = genVar name
genExpression (ExprBinary _ op l r) = genBinary genExpression op l r
genExpression (ExprUnary _ op e) = genUnary genExpression op e
genExpression (ExprCall _ (ExprVar _ "show") [a]) = genShowCall genExpression a
genExpression (ExprCall _ (ExprVar _ "error") [a]) = genErrorCall genExpression a
genExpression (ExprCall _ (ExprVar _ name) args) = genCall genExpression name args
genExpression (ExprCall {}) = error "Invalid function call target"
genExpression (ExprAccess _ t f) = genAccess genExpression t f
genExpression (ExprStructInit _ name fields) = genStructInit genExpression name fields
genExpression (ExprLitArray _ exprs) = genLitArray genExpression exprs
genExpression (ExprIndex _ target idx) = genIndex genExpression target idx
genExpression (ExprCast _ expr typ) = genCast genExpression expr typ
genExpression (ExprSizeof _ val) = genSizeof val

--
-- private
--

genVar :: String -> IRGen ([IRInstruction], IROperand, IRType)
genVar name = do
  symTable <- gets gsSymTable
  case Map.lookup name symTable of
    Just (op, typ) -> return ([], op, typ)
    Nothing -> throwError $ "genVar: variable not found in symbol table: " <> name

genSizeof :: Either Type Expression -> IRGen ([IRInstruction], IROperand, IRType)
genSizeof val = do

  targetIRType <- case val of
    Left astType -> pure $ astTypeToIRType astType
    Right expr   -> do
      (_, _, t) <- genExpression expr
      pure $ normalizePtr t

  structs <- gets gsStructs
  let size = sizeOfIRType structs targetIRType

  return ([], IRConstInt size, IRU64)

  where

    normalizePtr :: IRType -> IRType
    normalizePtr (IRPtr (IRArray elemType len)) = IRArray elemType len
    normalizePtr t = t

genCast :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genCast genExpr (ExprIndex _ target idx) astType = do
  -- Special case: casting indexed string/pointer access to larger type
  -- This loads sizeof(targetType) bytes from ptr+offset
  (targetInstrs, targetOp, targetType) <- genExpr target
  (idxInstrs, idxOp, _) <- genExpr idx
  let resultType = astTypeToIRType astType
  
  -- Check if this is a pointer type being indexed and cast to u64/i64
  case (targetType, resultType) of
    (IRPtr _, t) | t `elem` [IRU64, IRI64] -> do
      -- Load 8 bytes from pointer at offset
      tempName <- newTemp "word" resultType
      let loadInstr = IRLOAD_OFFSET tempName targetOp idxOp resultType
      pure (targetInstrs <> idxInstrs <> [loadInstr], IRTemp tempName resultType, resultType)
    _ -> do
      -- Regular cast: evaluate the index expression then cast
      (instrs, op, _) <- genExpr (ExprIndex undefined target idx)
      pure (instrs, op, resultType)

genCast genExpr expr astType = do
  (instrs, op, srcType) <- genExpr expr
  let targetType = astTypeToIRType astType
  -- If types are the same, no cast needed
  if srcType == targetType
    then pure (instrs, op, targetType)
    else do
      tempName <- newTemp "cast" targetType
      let castInstr = IRCAST tempName op srcType targetType
      pure (instrs <> [castInstr], IRTemp tempName targetType, targetType)
