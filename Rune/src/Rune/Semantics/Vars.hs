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
verifScope (fs, vs) (StmtVarDecl var t e:stmts) =
    let expr_type   = typeOfExpr (fs, vs) e
        (vs', err)  = assignVarType vs var $ fromMaybe expr_type t 
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
    let expr_type   = typeOfExpr (fs, vs) start
        (vs', err)  = assignVarType vs var $ fromMaybe expr_type t 
        multipleType= checkMultipleType var t expr_type
    in  err <> multipleType 
    <>  verifExpr (fs, vs') start
    <>  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) (StmtFor var t Nothing end body:stmts) =
    let expr_type   = fromMaybe TypeAny t
        (vs', err)  = assignVarType vs var $ fromMaybe expr_type t 
        multipleType= checkMultipleType var t expr_type
    in  err <> multipleType 
    <>  verifExpr (fs, vs') end
    <>  verifScope (fs, vs') body
    <>  verifScope (fs, vs) stmts
verifScope (fs, vs) (StmtForEach var t iterable body:stmts) =
    let expr_type   = typeOfExpr (fs, vs) iterable
        (vs', err)  = assignVarType vs var $ fromMaybe expr_type t 
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
    let (vs', err) = assignVarType vs lv $ typeOfExpr (fs, vs) rv
    in  err
    <>  verifExpr (fs, vs) rv
    <>  verifScope (fs, vs') stmts
verifScope s (StmtAssignment _ _:stmts) = verifScope s stmts
verifScope s (StmtStop:stmts) = verifScope s stmts
verifScope s (StmtNext:stmts) = verifScope s stmts
verifScope _ [] = Nothing

verifExpr :: Stack -> Expression -> Maybe String
verifExpr s (ExprBinary _ l r) = verifExpr s l <> verifExpr s r
verifExpr s (ExprCall _ args) = foldMap (verifExpr s) args
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
    fromMaybe TypeAny (HM.lookup name vs)

assignVarType :: VarStack -> String -> Type -> (VarStack, Maybe String)
assignVarType s v t =
    let msg = "\n\tTypeOverwrite: " ++ v
          ++ " has alerady type " ++ show t
        updated = (HM.insert v t s, Nothing)
        unchanged = (s, Nothing)
    in case HM.lookup v s of
      Nothing       -> updated
      Just TypeAny  -> updated
      Just t' | t' == t         -> updated
              | t' == TypeAny   -> updated
              | t' == TypeNull  -> updated
              | t  == TypeAny   -> unchanged
              | otherwise       -> (s, Just msg)

checkMultipleType :: String -> Maybe Type -> Type -> Maybe String
checkMultipleType v t e_t = 
    let msg = "\n\tMultipleType: " ++ v
          ++ " is " ++ show t ++ " and "
          ++ show e_t
    in case t of
        Nothing -> Nothing
        Just t' | t'  == e_t      -> Nothing
                | t'  == TypeAny  -> Nothing
                | e_t == TypeAny  -> Nothing
                | e_t == TypeNull -> Nothing
                | otherwise       -> Just msg
