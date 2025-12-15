{-# LANGUAGE TupleSections #-}

module Rune.Semantics.Vars (verifVars) where

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)

import Rune.Semantics.Type
  ( FuncStack
  , Stack
  )

import Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , SemanticError(..)
  , formatSemanticError
  )

verifVars :: Program -> Either String Program
verifVars prog@(Program n defs) = do
  fs        <- findFunc prog
  defs'     <- mapM (verifDefs fs) defs
  pure $ Program n defs'

-- Convert SemanticError to String
convertError :: Either SemanticError a -> Either String a
convertError (Left err) = Left (formatSemanticError err)
convertError (Right x) = Right x

verifDefs :: FuncStack -> TopLevelDef -> Either String TopLevelDef
-- r_t : return type
verifDefs fs (DefFunction name params r_t body) = do
  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
  body'     <- verifScope (fs, vs) body
  pure $ DefFunction name params r_t body'

verifDefs fs (DefOverride name params r_t body) = do
  let paramTypes = map paramType params
  let name' = mangleName name r_t paramTypes
  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
  body'     <- verifScope (fs, vs) body
  pure $ DefOverride name' params r_t body'

-- struct somewhat

verifDefs _ def = Right def


verifScope :: Stack -> Block -> Either String Block
-- v : variable   t : type
-- e : expression e_t : expression type
-- a, b : if cond then a else b
verifScope s@(fs, vs) (StmtVarDecl pos v t e : stmts) = do
  let e_t = exprType s e
      SourcePos file line col = pos
  vs'     <- convertError $ assignVarType vs v file line col e_t
  t'      <- convertError $ checkMultipleType v file line col t e_t
  e'      <- verifExpr s e
  stmts'  <- verifScope (fs, vs') stmts
  pure $ StmtVarDecl pos v (Just t') e' : stmts'

verifScope s (StmtExpr pos e : stmts) = do
  e'      <- verifExpr s e
  stmts'  <- verifScope s stmts
  pure $ StmtExpr pos e' : stmts'

verifScope s (StmtReturn pos (Just e) : stmts) = do
  e'      <- verifExpr s e
  stmts'  <- verifScope s stmts
  pure $ StmtReturn pos (Just e') : stmts'

verifScope s (StmtReturn pos Nothing : stmts) = do
  stmts'  <- verifScope s stmts
  pure $ StmtReturn pos (Just (ExprLitNull pos)) : stmts'

verifScope s (StmtIf pos cond a (Just b) : stmts) = do
  cond'   <- verifExpr s cond
  a'      <- verifScope s a
  b'      <- verifScope s b
  stmts'  <- verifScope s stmts
  pure $ StmtIf pos cond' a' (Just b') : stmts'

verifScope s (StmtIf pos cond a Nothing : stmts) = do
  cond'   <- verifExpr s cond
  a'      <- verifScope s a
  stmts'  <- verifScope s stmts
  pure $ StmtIf pos cond' a' Nothing : stmts'

verifScope s@(fs, vs) (StmtFor pos v t (Just start) end body : stmts) = do
  let e_t = exprType s start
      SourcePos file line col = pos
  vs'     <- convertError $ assignVarType vs v file line col e_t
  t'      <- convertError $ checkMultipleType v file line col t e_t
  start'  <- verifExpr (fs, vs') start
  end'    <- verifExpr (fs, vs') end
  body'   <- verifScope (fs, vs') body
  stmts'  <- verifScope s stmts
  pure $ StmtFor pos v (Just t') (Just start') end' body' : stmts'

verifScope s@(fs, vs) (StmtFor pos v t Nothing end body : stmts) = do
  let e_t = fromMaybe TypeAny t
      SourcePos file line col = pos
  vs'     <- convertError $ assignVarType vs v file line col e_t
  t'      <- convertError $ checkMultipleType v file line col t e_t
  end'    <- verifExpr (fs, vs') end
  body'   <- verifScope (fs, vs') body
  stmts'  <- verifScope s stmts
  pure $ StmtFor pos v (Just t') Nothing end' body' : stmts'

verifScope s@(fs, vs) (StmtForEach pos v t iter body : stmts) = do
  let e_t = exprType s iter
      SourcePos file line col = pos
  vs'     <- convertError $ assignVarType vs v file line col e_t
  t'      <- convertError $ checkMultipleType v file line col t e_t
  iter'   <- verifExpr (fs, vs') iter
  body'   <- verifScope (fs, vs') body
  stmts'  <- verifScope s stmts
  pure $ StmtForEach pos v (Just t') iter' body' : stmts'

verifScope s (StmtLoop pos body : stmts) = do
  body'   <- verifScope s body
  stmts'  <- verifScope s stmts
  pure $ StmtLoop pos body' : stmts'

-- bit weird i don't know if it work like this
verifScope s@(fs, vs) (StmtAssignment pos (ExprVar vpos lv) rv : stmts) = do
  let e_t = exprType s rv
      SourcePos file line col = pos
  vs'     <- convertError $ assignVarType vs lv file line col e_t
  rv'     <- verifExpr s rv
  stmts'  <- verifScope (fs, vs') stmts
  pure $ StmtAssignment pos (ExprVar vpos lv) rv' : stmts'

-- bit weird i don't know if it work like this
verifScope s (StmtAssignment pos lhs rv : stmts) = do
  lhs'    <- verifExpr s lhs
  rv'     <- verifExpr s rv
  stmts'  <- verifScope s stmts
  pure $ StmtAssignment pos lhs' rv' : stmts'

verifScope s (StmtStop pos : stmts) = do
  stmts'  <- verifScope s stmts
  pure $ StmtStop pos : stmts'

verifScope s (StmtNext pos : stmts) = do
  stmts'  <- verifScope s stmts
  pure $ StmtNext pos : stmts'

verifScope _ [] = Right []


verifExpr :: Stack -> Expression -> Either String Expression

verifExpr s (ExprUnary pos op val) = do
  val'    <- verifExpr s val
  pure $ ExprUnary pos op val'

verifExpr s (ExprBinary pos op l r) = do
  l'      <- verifExpr s l
  r'      <- verifExpr s r
  pure $ ExprBinary pos op l' r'

verifExpr s (ExprCall pos name args) = do
  let SourcePos file line col = pos
  name'   <- convertError $ checkParamType s name file line col args
  args'   <- mapM (verifExpr s) args
  pure $ ExprCall pos name' args'

-- linked to the struct
verifExpr s (ExprStructInit pos name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExpr s e) fields
  pure $ ExprStructInit pos name fields'

-- linked to the struct
verifExpr s (ExprAccess pos target field) = do
  target' <- verifExpr s target
  pure $ ExprAccess pos target' field

verifExpr (_, vs) (ExprVar pos var) =
  let SourcePos file line col = pos
  in if HM.member var vs
    then pure (ExprVar pos var)
    else Left $ formatSemanticError $ SemanticError
           file line col
           (printf "variable '%s' to be defined" var)
           "undefined variable"
           ["variable reference", "global context"]

-- maybe other cases i just don't try for now
verifExpr _ expr = Right expr

