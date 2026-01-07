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
import Rune.IR.IRHelpers (astTypeToIRType)

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
genExpression (ExprCall _ "show" [a]) = genShowCall genExpression a
genExpression (ExprCall _ "error" [a]) = genErrorCall genExpression a
genExpression (ExprCall _ name args) = genCall genExpression name args
genExpression (ExprAccess _ t f) = genAccess genExpression t f
genExpression (ExprStructInit _ name fields) = genStructInit genExpression name fields
genExpression (ExprLitArray _ exprs) = genLitArray genExpression exprs
genExpression (ExprIndex _ target idx) = genIndex genExpression target idx
genExpression (ExprCast _ expr typ) = genCast genExpression expr typ

--
-- private
--

genVar :: String -> IRGen ([IRInstruction], IROperand, IRType)
genVar name = do
  symTable <- gets gsSymTable
  case Map.lookup name symTable of
    Just (op, typ) -> return ([], op, typ)
    Nothing -> throwError $ "genVar: variable not found in symbol table: " <> name

genCast :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genCast genExpr expr astType = do
  (instrs, op, _) <- genExpr expr
  let targetType = astTypeToIRType astType
  pure (instrs, op, targetType)
