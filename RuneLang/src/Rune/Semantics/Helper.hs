module Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , selectSignature
  , checkEachParam
  , fixSelfType
  , SemanticError(..)
  , formatSemanticError
  ) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM

import Text.Printf (printf)

import Rune.AST.Nodes

import Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , StructStack
  , Stack
  )
import Rune.Semantics.OpType (isIntegerType, isFloatType, iHTBinary)

-- | Semantic error with location information
data SemanticError = SemanticError
  { seFile     :: String
  , seLine     :: Int
  , seCol      :: Int
  , seExpected :: String
  , seGot      :: String
  , seContext  :: [String]
  } deriving (Show, Eq)

-- | Format semantic error to match AST parser convention
formatSemanticError :: SemanticError -> String
formatSemanticError (SemanticError file line col expected got ctx) =
  let header = printf "[ERROR]: %s:%d:%d: error:" file line col
      expectedLine = "  Expected: " ++ expected
      gotLine = "  Got: " ++ got
      contexts = map (\c -> "  ... in " ++ c) ctx
  in intercalate "\n" ([header, expectedLine, gotLine] ++ contexts)

checkParamType :: Stack -> String -> String -> Int -> Int -> [Expression] -> Either SemanticError String
checkParamType s@(fs, _, _) fname file line col es =
  let mkError expected got = SemanticError file line col expected got ["function call", "global context"]
  in case HM.lookup fname fs of
    Nothing         -> Left $ mkError ("function '" ++ fname ++ "' to exist") "undefined function"
    Just []         -> Left $ mkError ("function '" ++ fname ++ "' to exist") "undefined function"
    Just [sig]      -> checkSingle sig
    Just sigs       -> checkAll (mkError (printf "matching signature for %s with arguments %s" fname (show $ map (exprType s) es)) "no matching overload") sigs
  where
    checkSingle :: (Type, [Type]) -> Either SemanticError String
    checkSingle (_, at) =
      case checkEachParam s file line col 0 es at of
        Nothing  -> Right fname
        Just err -> Left err

    -- if multiple signature so mangle name
    checkAll :: SemanticError -> [(Type, [Type])] -> Either SemanticError String
    checkAll err_msg [] = Left err_msg
    checkAll err_msg ((ret, at):rest) =
      case checkEachParam s file line col 0 es at of
        Nothing  -> Right $ mangleName fname ret at
        Just _   -> checkAll err_msg rest

mangleName :: String -> Type -> [Type] -> String
mangleName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = intercalate "_" (show ret : fname : map show args)

selectSignature :: FuncStack -> String -> [Type] -> Maybe Type
selectSignature fs name at =
  case HM.lookup name fs of
    Nothing   -> Nothing
    Just []   -> Nothing
    Just sigs ->
      case filter (match at) sigs of
        [(rt, _)] -> Just rt
        _         -> Nothing
  where
    match :: [Type] -> (Type, [Type]) -> Bool
    match act (_, expec) =
      length act == length expec &&
      and (zipWith compat expec act)
    compat TypeAny _ = True
    compat a b = a == b

exprType :: Stack -> Expression -> Type
exprType _ (ExprLitInt _ _) = TypeI32
exprType _ (ExprLitFloat _ _) = TypeF32
exprType _ (ExprLitString _ _) = TypeString
exprType _ (ExprLitChar _ _) = TypeU8
exprType _ (ExprLitBool _ _) = TypeBool
exprType _ (ExprStructInit _ st _) = TypeCustom st
exprType _ (ExprLitNull _) = TypeNull
exprType s (ExprAccess pos target field) =
  let targetType = exprType s target
      ss = case s of (_, _, ss') -> ss'
  in case getFieldType pos ss targetType field of
       Right t -> t
       Left _ -> TypeAny
exprType s (ExprBinary _ op l r) =
  let leftType = exprType s l
      rightType = exprType s r
  in case iHTBinary op leftType rightType of
       Right t -> t
       Left _ -> leftType  -- fallback to left type if error
exprType s (ExprUnary _ _ expr) = exprType s expr -- assume the op don't change the type
exprType (_, vs, _) (ExprVar _ name) = fromMaybe TypeAny (HM.lookup name vs)
exprType s@(fs, _, _) (ExprCall _ (ExprVar _ name) args) =
  let argTypes = map (exprType s) args
  in case selectSignature fs name argTypes of
    Just ret -> ret
    Nothing  -> TypeAny
exprType _ (ExprCall _ _ _) = TypeAny

assignVarType :: VarStack -> String -> String -> Int -> Int -> Type -> Either SemanticError VarStack
assignVarType vs _ _ _ _ TypeAny = Right vs
assignVarType vs v file line col t =
  let updated = Right $ HM.insert v t vs
  in case HM.lookup v vs of
    Nothing       -> updated
    Just TypeAny  -> updated
    Just TypeNull -> updated
    Just t' | t' == t   -> updated
            | otherwise -> Left $ SemanticError file line col (printf "variable '%s' to have type %s" v (show t')) (printf "type %s" (show t)) ["variable assignment", "global context"]

checkMultipleType :: String -> String -> Int -> Int -> Maybe Type -> Type -> Either SemanticError Type
checkMultipleType _ _ _ _ Nothing e_t         = Right e_t
checkMultipleType _ _ _ _ (Just TypeAny) e_t  = Right e_t
checkMultipleType _ _ _ _ (Just t) TypeAny    = Right t
checkMultipleType _ _ _ _ _ TypeNull          = Right TypeNull
checkMultipleType v file line col (Just t) e_t
  | t == e_t  = Right t
  -- Allow implicit conversion between integer types
  | isIntegerType t && isIntegerType e_t = Right t
  -- Allow implicit conversion between float types
  | isFloatType t && isFloatType e_t = Right t
  | otherwise = Left $ SemanticError file line col (printf "variable '%s' to have type %s" v (show t)) (printf "type %s being assigned" (show e_t)) ["type check", "global context"]

checkEachParam :: Stack -> String -> Int -> Int -> Int -> [Expression] -> [Type] -> Maybe SemanticError
checkEachParam s file line col i (_:es) (TypeAny:at) = checkEachParam s file line col (i + 1) es at
checkEachParam s file line col i (e:es) (t:at) =
  let expected = printf "argument %d to have type %s" i (show t)
      got = printf "type %s" (show $ exprType s e)
  in if exprType s e == t
     then checkEachParam s file line col (i + 1) es at
     else Just $ SemanticError file line col expected got ["parameter check", "function call", "global context"]
checkEachParam _ _ _ _ _ [] [] = Nothing
checkEachParam _ file line col i [] at =
  let expected = printf "%d arguments" (length at + i)
      got = printf "%d arguments (too few)" i
  in Just $ SemanticError file line col expected got ["parameter count", "function call", "global context"]
checkEachParam _ file line col i es [] =
  let expected = printf "%d arguments" i
      got = printf "%d arguments (too many)" (length es + i)
  in Just $ SemanticError file line col expected got ["parameter count", "function call", "global context"]

getFieldType :: SourcePos -> StructStack -> Type -> String -> Either SemanticError Type
getFieldType pos ss (TypeCustom sName) fldName =
  let SourcePos file line col = pos
      mkError expected got = SemanticError file line col expected got ["field access", "global context"]
  in case HM.lookup sName ss of
    Nothing -> Left $ mkError (printf "struct '%s' to exist" sName) "undefined struct"
    Just (DefStruct _ fields _) ->
      case filter (\(Field fName _) -> fName == fldName) fields of
        [] -> Left $ mkError (printf "field '%s' to exist in struct '%s'" fldName sName) "undefined field"
        (Field _ t:_) -> Right t
    Just _ -> Left $ mkError (printf "struct '%s' to be a valid struct definition" sName) "not a struct definition"
getFieldType pos _ otherType fldName =
  let SourcePos file line col = pos
  in Left $ SemanticError file line col 
    (printf "field access to be valid on type %s" (show otherType))
    (printf "cannot access field '%s' on type '%s'" fldName (show otherType))
    ["field access", "global context"]

fixSelfType :: String -> [Parameter] -> [Parameter]
fixSelfType sName (p:rest)
  | paramName p == "self" = p { paramType = TypeCustom sName } : rest
fixSelfType _ params = params
