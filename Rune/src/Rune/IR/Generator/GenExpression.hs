module Rune.IR.Generator.GenExpression (genExpression) where

import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map
import Rune.AST.Nodes (Expression (..))
import Rune.IR.Generator.Expression.Binary (genBinary)
import Rune.IR.Generator.Expression.Call (genCall)
import Rune.IR.Generator.Expression.Call.Show (genShowCall)
import Rune.IR.Generator.Expression.Literals
import Rune.IR.Generator.Expression.Struct (genAccess, genStructInit)
import Rune.IR.Generator.Expression.Unary (genUnary)
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- public
--

genExpression :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExpression (ExprLitInt n) = genLitInt n
genExpression (ExprLitFloat f) = genLitFloat f
genExpression (ExprLitChar c) = genLitChar c
genExpression (ExprLitBool b) = genLitBool b
genExpression (ExprLitNull) = genLitNull
genExpression (ExprLitString s) = genLitString s
genExpression (ExprVar name) = genVar name
genExpression (ExprBinary op l r) = genBinary genExpression op l r
genExpression (ExprUnary op e) = genUnary genExpression op e
genExpression (ExprCall "show" [a]) = genShowCall genExpression a
genExpression (ExprCall name args) = genCall genExpression name args
genExpression (ExprAccess t f) = genAccess genExpression t f
genExpression (ExprStructInit name fields) = genStructInit genExpression name fields

--
-- private
--

genVar :: String -> IRGen ([IRInstruction], IROperand, IRType)
genVar name = do
  symTable <- gets gsSymTable
  case Map.lookup name symTable of
    Just (op, typ) -> return ([], op, typ)
    Nothing -> error $ "genVar: variable not found in symbol table: " ++ name
