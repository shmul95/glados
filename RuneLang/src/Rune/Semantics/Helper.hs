{-# LANGUAGE LambdaCase #-}

module Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , selectSignature
  , checkEachParam
  , isTypeCompatible
  , SemanticError(..)
  , formatSemanticError
  , getFieldType
  , fixSelfType
  , hasVariadicParam
  , isVariadicParam
  , unwrapVariadic
  , isRefParam
  , unwrapRef
  ) where

import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Text.Printf (printf)

import Rune.Semantics.Type
import Rune.Semantics.OpType
import Rune.AST.Nodes

--
-- error
--

data SemanticError = SemanticError
  { seFile     :: String
  , seLine     :: Int
  , seCol      :: Int
  , seExpected :: String
  , seGot      :: String
  , seContext  :: [String]
  } deriving (Show, Eq)

formatSemanticError :: SemanticError -> String
formatSemanticError (SemanticError file line col expected got ctx) =
  let header = printf "[ERROR]: %s:%d:%d: error:" file line col
  in intercalate "\n"
       ([ header
        , "  Expected: " <> expected
        , "  Got: " <> got
        ] <> map ("  ... in " <>) ctx)

--
-- variadic
--

hasVariadicParam :: [Parameter] -> Bool
hasVariadicParam = any (isVariadicParam . paramType)

isVariadicParam :: Type -> Bool
isVariadicParam (TypeVariadic _) = True
isVariadicParam _                = False

unwrapVariadic :: Type -> Type
unwrapVariadic (TypeVariadic t) = t
unwrapVariadic t                = t

isRefParam :: Type -> Bool
isRefParam (TypeRef _) = True
isRefParam _ = False

unwrapRef :: Type -> Type
unwrapRef (TypeRef t) = t
unwrapRef t = t

--
-- name mangling
--

mangleName :: String -> Type -> [Type] -> String
mangleName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = intercalate "_" (show ret : fname : map show args)

isRightFunction :: (String, [Type]) -> String -> (Type, [Type]) -> Bool
isRightFunction (fname, argTypes) name (ret, _) =
  name == mangleName fname ret argTypes

isCompatibleMangling :: String -> String -> Type -> [Type] -> [Type] -> Bool
isCompatibleMangling baseName fname ret paramTypes argTypes =
  let mangledName = mangleName baseName ret paramTypes
      nameOk = fname == baseName || fname == mangledName
      argsOk =
           length paramTypes == length argTypes
        && all (uncurry isTypeCompatible) (zip paramTypes argTypes)
  in nameOk && argsOk

--
-- param checking
--

paramTypeError
  :: String -> Int -> Int -> Int -> Type -> Type -> SemanticError
paramTypeError file line col i expected got =
  SemanticError
    file line col
    (printf "argument %d to have type %s" i (show expected))
    (printf "type %s" (show got))
    ["parameter check", "function call", "global context"]

checkEachParam
  :: Stack -> String -> Int -> Int -> Int
  -> [Expression] -> [Parameter]
  -> Maybe SemanticError

checkEachParam _ _ _ _ _ [] [] = Nothing

-- only a variadic parameter remains
checkEachParam _ _ _ _ _ [] [p]
  | isVariadicParam (paramType p) = Nothing

-- missing arguments
checkEachParam _ file line col i [] ps
  | all (isJust . paramDefault) ps = Nothing
  | otherwise =
      Just $ SemanticError
        file line col
        (printf "%d arguments" (length ps + i))
        (printf "%d arguments (too few)" i)
        ["parameter count", "function call", "global context"]

-- too many arguments (should NEVER mais alors NEVER happen)
checkEachParam _ _ _ _ _ _ [] = Nothing

checkEachParam s file line col i (e:es) (p:ps) =
  case exprType s e of
    Left err ->
      Just $ SemanticError
        file line col
        "valid expression type"
        err
        ["parameter check", "function call"]

    Right actualType ->
      let expectedType = paramType p
          ok = isTypeCompatible (unwrapVariadic expectedType) actualType
      in if ok
         then
           if isVariadicParam expectedType
           then checkEachParam s file line col (i + 1) es [p]
           else checkEachParam s file line col (i + 1) es ps
         else
           Just $ paramTypeError file line col i expectedType actualType

--
-- function call resolution
--

checkParamType
  :: Stack -> (String, [Type])
  -> String -> Int -> Int
  -> [Expression]
  -> Either SemanticError String
checkParamType s@(fs, _, _) (fname, argTypes) file line col es =
  let mkError e g =
        SemanticError file line col e g ["function call", "global context"]

      exact =
        HM.toList $
          HM.filterWithKey
            (\k (ret, ps) ->
              isRightFunction (fname, argTypes) k (ret, map paramType ps))
            fs

      compatible =
        HM.toList $
          HM.filterWithKey
            (\k (ret, ps) ->
              isCompatibleMangling fname k ret (map paramType ps) argTypes)
            fs

      candidates =
        case (exact, compatible, HM.lookup fname fs) of
          (x@(_:_), _, _) -> x
          ([], x@(_:_), _) -> x
          ([], [], Just sig) -> [(fname, sig)]
          _ -> []

  in case candidates of
    [] ->
      Left $ mkError ("function '" <> fname <> "' to exist") "undefined function"

    [(name, (_, ps))] ->
      maybe (Right name) Left $
        checkEachParam s file line col 0 es ps

    multiple ->
      case filter (\(_, (_, ps)) ->
                    isNothing $ checkEachParam s file line col 0 es ps) multiple of
        [(name, _)] -> Right name
        [] -> Left $ mkError
                ("function '" <> fname <> "' with compatible arguments")
                "no matching signature"
        _  -> Left $ mkError
                (printf "multiple signatures for %s" fname)
                "ambiguous function call"

--
-- expression typing
--

exprType :: Stack -> Expression -> Either String Type
exprType _ (ExprLitInt _ _)        = Right TypeI32
exprType _ (ExprLitFloat _ _)      = Right TypeF32
exprType _ (ExprLitString _ _)     = Right TypeString
exprType _ (ExprLitChar _ _)       = Right TypeChar
exprType _ (ExprLitBool _ _)       = Right TypeBool
exprType _ (ExprLitNull _)         = Right TypeNull
exprType _ (ExprStructInit _ s _)  = Right $ TypeCustom s
exprType _ (ExprCast _ _ t)        = Right t
exprType _ (ExprSizeof _ _)        = Right TypeU64

exprType s (ExprUnary _ op e) = case op of
  Deref -> exprType s e >>= \case
    TypePtr t -> Right t
    t -> Left $ "Cannot dereference non-pointer type: " ++ show t
  Reference -> do
    t <- exprType s e
    Right $ TypePtr t
  _ -> exprType s e

exprType s (ExprBinary _ op a b) = do
  ta <- exprType s a
  tb <- exprType s b
  iHTBinary op ta tb

exprType (_, vs, _) (ExprVar _ v) =
  Right $ fromMaybe TypeAny (HM.lookup v vs)

exprType s@(fs, _, _) (ExprCall _ (ExprVar _ fn) args) = do
  argTypes <- mapM (exprType s) args
  Right $ fromMaybe TypeAny (selectSignature fs fn argTypes)

exprType _ (ExprCall {}) =
  Right TypeAny

exprType s (ExprIndex _ target _) =
  exprType s target >>= \case
    TypeArray t -> Right t
    TypeString  -> Right TypeChar
    TypePtr t   -> Right t
    TypeAny     -> Right TypeAny
    t           -> Left $ printf "cannot index type %s" (show t)

exprType _ (ExprLitArray _ []) =
  Right $ TypeArray TypeAny

exprType s (ExprLitArray _ (e:es)) = do
  t  <- exprType s e
  ts <- mapM (exprType s) es
  if all (isTypeCompatible t) ts
  then Right $ TypeArray t
  else Left $ printf "incompatible array element types, expected %s" (show t)

exprType s (ExprAccess pos target field) = do
  t <- exprType s target
  let (_, _, ss) = s
  either (Left . formatSemanticError) Right $
    getFieldType pos ss t field

--
-- assignment / compatibility
--

assignVarType
  :: VarStack -> String
  -> String -> Int -> Int
  -> Type
  -> Either SemanticError VarStack
assignVarType vs v file line col t =
  case HM.lookup v vs of
    Nothing       -> Right $ HM.insert v t vs
    Just TypeAny  -> Right $ HM.insert v t vs
    Just TypeNull -> Right $ HM.insert v t vs
    Just t'
      | isTypeCompatible t t' ->
          Right $ HM.insert v t vs
      | otherwise ->
          Left $ SemanticError
            file line col
            (printf "variable '%s' to have type %s" v (show t'))
            (printf "type %s" (show t))
            ["variable assignment", "global context"]

isTypeCompatible :: Type -> Type -> Bool
isTypeCompatible TypeAny _ = True
isTypeCompatible _ TypeAny = True
isTypeCompatible (TypePtr TypeAny) _ = True
isTypeCompatible _ (TypePtr TypeAny) = True
isTypeCompatible (TypePtr _) TypeNull = True
isTypeCompatible TypeNull (TypePtr _) = True
isTypeCompatible (TypePtr TypeChar) TypeString = True
isTypeCompatible TypeString (TypePtr TypeChar) = True
isTypeCompatible (TypePtr a) (TypePtr b) = isTypeCompatible a b
isTypeCompatible (TypeRef a) b = isTypeCompatible a b
isTypeCompatible a (TypeRef b) = isTypeCompatible a b
isTypeCompatible (TypeVariadic a) b = isTypeCompatible a b
isTypeCompatible a b
  | sameType a b = True
  | isIntegerType a && isIntegerType b = True
  | isFloatType a && isFloatType b = True
  | otherwise = False

--
-- helpers
--

checkMultipleType
  :: String -> String -> Int -> Int
  -> Maybe Type -> Type
  -> Either SemanticError Type
checkMultipleType _ _ _ _ Nothing t = Right t
checkMultipleType _ _ _ _ (Just TypeAny) t = Right t
checkMultipleType _ _ _ _ (Just (TypeArray TypeAny)) (TypeArray t) = Right (TypeArray t)
checkMultipleType _ _ _ _ (Just t) TypeAny = Right t
checkMultipleType _ _ _ _ _ TypeNull = Right TypeNull
checkMultipleType v file line col (Just t) e
  | isTypeCompatible t e = Right t
  | otherwise =
      Left $ SemanticError
        file line col
        (printf "variable '%s' to have type %s" v (show t))
        (printf "type %s being assigned" (show e))
        ["type check", "global context"]

selectSignature :: FuncStack -> String -> [Type] -> Maybe Type
selectSignature fs name argTypes =
  case HM.toList $
       HM.filterWithKey
         (\k (ret, ps) ->
           isRightFunction (name, argTypes) k (ret, map paramType ps))
         fs of
    ((_, (ret, _)) : _) -> Just ret
    [] -> fmap fst (HM.lookup name fs)

getFieldType :: SourcePos -> StructStack -> Type -> String -> Either SemanticError Type
getFieldType (SourcePos file line col) ss (TypeCustom s) field =
  case HM.lookup s ss of
    Nothing ->
      Left $ SemanticError
        file line col
        (printf "struct '%s' to exist" s)
        "undefined struct"
        ["field access", "global context"]

    Just (DefStruct _ fields _) ->
      case [ t | Field f t <- fields, f == field ] of
        (t:_) -> Right t
        [] ->
          Left $ SemanticError
            file line col
            (printf "field '%s' to exist in struct '%s'" field s)
            "undefined field"
            ["field access", "global context"]

    _ ->
      Left $ SemanticError
        file line col
        (printf "struct '%s' to be a valid struct definition" s)
        "not a struct definition"
        ["field access", "global context"]

getFieldType (SourcePos file line col) _ t field =
  Left $ SemanticError
    file line col
    (printf "field access to be valid on type %s" (show t))
    (printf "cannot access field '%s' on type '%s'" field (show t))
    ["field access", "global context"]

fixSelfType :: String -> [Parameter] -> [Parameter]
fixSelfType s (p:ps)
  | paramName p == "self" =
      p { paramType = TypeCustom s } : ps
fixSelfType _ ps = ps
