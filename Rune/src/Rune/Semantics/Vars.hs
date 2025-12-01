module Rune.Semantics.Vars ( verifVars ) where

import Rune.AST.Nodes
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Rune.Semantics.Func (findFunc, FuncStack)

type VarStack = HashMap String Type
type Stack = (FuncStack, VarStack)

-- if Nothing everything good else Error message
verifVars :: Program -> Maybe String
verifVars (Program n defs) =
    foldMap (verifDefs (findFunc (Program n defs))) defs

verifDefs :: FuncStack -> TopLevelDef -> Maybe String
verifDefs fs (DefFunction _ params _ body) = verifScope (fs, HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
verifDefs fs (DefOverride _ params _ body) = verifScope (fs, HM.fromList (map (\p -> (paramName p, paramType p)) params)) body
-- verifDefs fs (DefStruct _ attr _) = undefined -- when there will be type and all
verifDefs _ _ = Nothing


verifScope :: Stack -> Block -> Maybe String
-- name n, type t, expr e
verifScope (fs, vs) ((StmtVarDecl n t e):stmts)
    = let vs' = (HM.insert n (fromMaybe (typeOfExpr (fs, vs) e) t) vs)
    in  verifExpr (fs, vs) e
    <>  verifScope (fs, vs') stmts
verifScope s ((StmtReturn e):stmts)
    =   maybe Nothing (verifExpr s) e
    <>  verifScope s stmts
verifScope s ((StmtIf cond thenA elseB):stmts)
    =   verifExpr s cond
    <>  verifScope s thenA
    <>  maybe Nothing (verifScope s) elseB
    <>  verifScope s stmts
verifScope (fs, vs) ((StmtFor var t (Just start) end body):stmts)
    = let vs' = HM.insert var (fromMaybe (typeOfExpr (fs, vs) start) t) vs
    in  verifExpr (fs, vs') start
    <>  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) ((StmtFor var t Nothing end body):stmts)
    = let vs' = HM.insert var (fromMaybe TypeAny t) vs
    in  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) ((StmtForEach var t iterable body):stmts)
    = let vs' = HM.insert var (fromMaybe (typeOfExpr (fs, vs) iterable) t) vs
    in  verifExpr (fs, vs') iterable
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope s ((StmtExpr e):stmts) 
    =   verifExpr s e
    <>  verifScope s stmts
verifScope _ [] = Nothing
verifScope _ _ = Nothing -- TODO not managed pattern for now 


verifExpr :: Stack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = (verifExpr s l) <> (verifExpr s r)
verifExpr s (ExprCall _ args) = foldMap (verifExpr s) args
verifExpr s (ExprStructInit _ fields) = foldMap (verifExpr s . snd) fields
verifExpr s (ExprAccess target _) = verifExpr s target
verifExpr s (ExprUnary _ val) = verifExpr s val
verifExpr s (ExprVar var) =
    case HM.member var (snd s) of
        True -> Nothing
        False -> Just $ "\n\t" ++ var ++ " : var doesn't exist in the scope"
verifExpr _ _ = Nothing


typeOfExpr :: Stack -> Expression -> Type
typeOfExpr _ (ExprLitInt _) = TypeI32
typeOfExpr _ (ExprLitFloat  _) = TypeF32
typeOfExpr _ (ExprLitString  _) = TypeString
typeOfExpr _ (ExprLitChar _ ) = TypeU8
typeOfExpr _ (ExprLitBool  _) = TypeBool
typeOfExpr _ (ExprStructInit st _) = TypeCustom st
typeOfExpr _ ExprLitNull = TypeNull
typeOfExpr _ (ExprAccess _ _) = TypeAny -- don't know how to use struct
typeOfExpr s (ExprBinary _ expr _) = typeOfExpr s expr -- assume both expr are of the same type
typeOfExpr s (ExprUnary _ expr) = typeOfExpr s expr -- assume the op don't change the type
typeOfExpr (fs, _) (ExprCall fn _) =
    case HM.lookup fn fs of
        Just (t, _) -> t
        Nothing -> TypeAny
typeOfExpr (_, vs) (ExprVar name) =
    case HM.lookup name vs of
        Just t -> t
        Nothing -> TypeAny

