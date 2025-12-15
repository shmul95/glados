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
  )

verifVars :: Program -> Either String (Program, FuncStack)
verifVars prog@(Program n defs) = do
  fs        <- findFunc prog
  defs'     <- mapM (verifDefs fs) defs
  let fs' = mangleFuncStack fs
  pure $ (Program n defs', fs')

mangleFuncStack :: FuncStack -> FuncStack
mangleFuncStack fs = HM.foldlWithKey' expandOverloads fs fs
  where
    expandOverloads acc name sigs
      | length sigs > 1 = foldr (addMangled name) acc sigs
      | otherwise = acc
    
    addMangled name (ret, args) acc =
        let mName = mangleName name ret args
        in HM.insert mName [(ret, args)] acc

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
verifScope s@(fs, vs) (StmtVarDecl v t e : stmts) = do
  e_t     <- exprType s e
  t'      <- checkMultipleType v t e_t
  vs'     <- assignVarType vs v t'
  e'      <- verifExpr s e
  stmts'  <- verifScope (fs, vs') stmts
  pure $ StmtVarDecl v (Just t') e' : stmts'

verifScope s (StmtExpr e : stmts) = do
  e'      <- verifExpr s e
  stmts'  <- verifScope s stmts
  pure $ StmtExpr e' : stmts'

verifScope s (StmtReturn (Just e) : stmts) = do
  e'      <- verifExpr s e
  stmts'  <- verifScope s stmts
  pure $ StmtReturn (Just e') : stmts'

verifScope s (StmtReturn Nothing : stmts) = do
  stmts'  <- verifScope s stmts
  pure $ StmtReturn (Just ExprLitNull) : stmts'

verifScope s (StmtIf cond a (Just b) : stmts) = do
  cond'   <- verifExpr s cond
  a'      <- verifScope s a
  b'      <- verifScope s b
  stmts'  <- verifScope s stmts
  pure $ StmtIf cond' a' (Just b') : stmts'
  
verifScope s (StmtIf cond a Nothing : stmts) = do
  cond'   <- verifExpr s cond
  a'      <- verifScope s a
  stmts'  <- verifScope s stmts
  pure $ StmtIf cond' a' Nothing : stmts'

verifScope s@(fs, vs) (StmtFor v t (Just start) end body : stmts) = do
  e_t     <- exprType s start
  vs'     <- assignVarType vs v e_t
  t'      <- checkMultipleType v t e_t
  start'  <- verifExpr (fs, vs') start
  end'    <- verifExpr (fs, vs') end
  body'   <- verifScope (fs, vs') body
  stmts'  <- verifScope s stmts
  pure $ StmtFor v (Just t') (Just start') end' body' : stmts'

verifScope s@(fs, vs) (StmtFor v t Nothing end body : stmts) = do
  let e_t = fromMaybe TypeAny t
  vs'     <- assignVarType vs v e_t
  t'      <- checkMultipleType v t e_t
  end'    <- verifExpr (fs, vs') end
  body'   <- verifScope (fs, vs') body
  stmts'  <- verifScope s stmts
  pure $ StmtFor v (Just t') Nothing end' body' : stmts'

verifScope s@(fs, vs) (StmtForEach v t iter body : stmts) = do
  e_t     <- exprType s iter
  vs'     <- assignVarType vs v e_t
  t'      <- checkMultipleType v t e_t
  iter'   <- verifExpr (fs, vs') iter
  body'   <- verifScope (fs, vs') body
  stmts'  <- verifScope s stmts
  pure $ StmtForEach v (Just t') iter' body' : stmts'

verifScope s (StmtLoop body : stmts) = do
  body'   <- verifScope s body
  stmts'  <- verifScope s stmts
  pure $ StmtLoop body' : stmts'

-- bit weird i don't know if it work like this
verifScope s@(fs, vs) (StmtAssignment (ExprVar lv) rv : stmts) = do
  e_t     <- exprType s rv
  vs'     <- assignVarType vs lv e_t
  rv'     <- verifExpr s rv
  stmts'  <- verifScope (fs, vs') stmts
  pure $ StmtAssignment (ExprVar lv) rv' : stmts'

-- bit weird i don't know if it work like this
verifScope s (StmtAssignment lhs rv : stmts) = do
  lhs'    <- verifExpr s lhs
  rv'     <- verifExpr s rv
  stmts'  <- verifScope s stmts
  pure $ StmtAssignment lhs' rv' : stmts'

verifScope s (StmtStop : stmts) = do
  stmts'  <- verifScope s stmts
  pure $ StmtStop : stmts'

verifScope s (StmtNext : stmts) = do
  stmts'  <- verifScope s stmts
  pure $ StmtNext : stmts'

verifScope _ [] = Right []


verifExpr :: Stack -> Expression -> Either String Expression

verifExpr s (ExprUnary op val) = do
  val'    <- verifExpr s val
  pure $ ExprUnary op val'

verifExpr s (ExprBinary op l r) = do
  l'      <- verifExpr s l
  r'      <- verifExpr s r
  pure $ ExprBinary op l' r'

verifExpr s (ExprCall name args) = do
  name'   <- checkParamType s name args
  args'   <- mapM (verifExpr s) args
  pure $ ExprCall name' args'
  
-- linked to the struct
verifExpr s (ExprStructInit name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExpr s e) fields
  pure $ ExprStructInit name fields'
  
-- linked to the struct
verifExpr s (ExprAccess target field) = do
  target' <- verifExpr s target
  pure $ ExprAccess target' field
  
verifExpr (_, vs) (ExprVar var) =
  let msg = "\n\tUndefinedVar: %s doesn't exist in the scope"
  in if HM.member var vs
    then pure (ExprVar var)
    else Left $ printf msg var
 
-- maybe other cases i just don't try for now
verifExpr _ expr = Right expr
