module Rune.Semantics.Vars (verifVars) where

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)

import Rune.Semantics.Type
  ( FuncStack
  , Stack
  )

import Rune.Semantics.Helper
  ( checkParamType
  , exprType
  , assignVarType
  , checkMultipleType
  )

-- if Nothing everything good else Error message
verifVars :: Program -> Maybe String
verifVars (Program n defs) = 
  case findFunc (Program n defs) of
    Left err -> Just err
    Right fs -> foldMap (verifDefs fs) defs

verifDefs :: FuncStack -> TopLevelDef -> Maybe String
verifDefs fs (DefFunction _ params _ body) = verifScope (fs, HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
verifDefs fs (DefOverride _ params _ body) = verifScope (fs, HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
-- verifDefs fs (DefStruct _ _ methods) = foldMap (verifDefs fs) methods
verifDefs _ _ = Nothing


verifScope :: Stack -> Block -> Maybe String
-- name n, type t, expr e
verifScope (fs, vs) (StmtVarDecl var t e:stmts) =
    let expr_type   = exprType (fs, vs) e
        (vs', err)  = assignVarType vs var expr_type 
        multipleType= checkMultipleType var t expr_type
    in  err <> multipleType 
    <>  verifExpr (fs, vs) e
    <>  verifScope (fs, vs') stmts
verifScope s (StmtReturn e:stmts) =
    (verifExpr s =<< e)
    <>  verifScope s stmts
verifScope s (StmtIf cond thenA elseB:stmts) =
    verifExpr s cond
    <>  verifScope s thenA
    <>  (verifScope s =<< elseB)
    <>  verifScope s stmts
verifScope (fs, vs) (StmtFor var t (Just start) end body:stmts) =
    let expr_type   = exprType (fs, vs) start
        (vs', err)  = assignVarType vs var expr_type 
        multipleType= checkMultipleType var t expr_type
    in  err <> multipleType 
    <>  verifExpr (fs, vs') start
    <>  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) (StmtFor var t Nothing end body:stmts) =
    let expr_type   = fromMaybe TypeAny t
        (vs', err)  = assignVarType vs var expr_type 
        multipleType= checkMultipleType var t expr_type
    in  err <> multipleType 
    <>  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) (StmtForEach var t iterable body:stmts) =
    let expr_type   = exprType (fs, vs) iterable
        (vs', err)  = assignVarType vs var expr_type 
        multipleType= checkMultipleType var t expr_type
    in  err <> multipleType 
    <>  verifExpr (fs, vs') iterable
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope s (StmtExpr e:stmts) =
    verifExpr s e
    <>  verifScope s stmts
verifScope s (StmtLoop block:stmts)
    =   verifScope s block
    <>  verifScope s stmts
-- bit weird i don't know if it work like this
verifScope (fs, vs) (StmtAssignment (ExprVar lv) rv:stmts) =
    let (vs', err) = assignVarType vs lv $ exprType (fs, vs) rv
    in  err
    <>  verifExpr (fs, vs) rv
    <>  verifScope (fs, vs') stmts
verifScope s (StmtAssignment lhs rv:stmts) =
    verifExpr s lhs <> verifExpr s rv <> verifScope s stmts

verifScope s (StmtStop:stmts) = verifScope s stmts
verifScope s (StmtNext:stmts) = verifScope s stmts
verifScope _ [] = Nothing

verifExpr :: Stack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = verifExpr s l <> verifExpr s r
verifExpr s (ExprCall name args) = checkParamType s name args <> foldMap (verifExpr s) args
verifExpr s (ExprStructInit _ fields) = foldMap (verifExpr s . snd) fields
verifExpr s (ExprAccess target _) = verifExpr s target
verifExpr s (ExprUnary _ val) = verifExpr s val
verifExpr s (ExprVar var) =
    let msg = "\n\tUndefinedVar: " ++ var
          ++ " doesn't exist in the scope"
    in case HM.member var (snd s) of
        True  -> Nothing
        False -> Just msg
verifExpr _ _ = Nothing

