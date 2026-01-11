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
  ) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM

import Text.Printf (printf)

import Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , StructStack
  , Stack
  )
import Rune.Semantics.OpType (isIntegerType, isFloatType, iHTBinary, sameType)
import Rune.AST.Nodes

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
      expectedLine = "  Expected: " <> expected
      gotLine = "  Got: " <> got
      contexts = map ("  ... in " <>) ctx
  in intercalate "\n" ([header, expectedLine, gotLine] <> contexts)


checkParamType :: Stack -> (String, [Type]) -> String -> Int -> Int -> [Expression] -> Either SemanticError String
checkParamType s@(fs, _, _) (fname, argTypes) file line col es =
  let mkError expected got = SemanticError file line col expected got ["function call", "global context"]
      -- Find candidates by exact match on mangled name
      exactMangled = HM.filterWithKey (\k (ret, args) -> isRightFunction (fname, argTypes) k (ret, args)) fs
      -- For struct method overrides (names containing _), also check compatible manglings
      compatibleMangled = if isStructMethod fname
                          then HM.toList $ HM.filterWithKey (\k (ret, args) -> isCompatibleMangling fname k ret args argTypes) fs
                          else []
      -- Also check direct name lookup
      candidates = case (HM.toList exactMangled, compatibleMangled, HM.lookup fname fs) of
        (m@(_:_), _, _) -> m                 -- Exact mangled match (priority)
        ([], c@(_:_), _) -> c                -- Compatible mangled match (struct methods only)
        ([], [], Just sig) -> [(fname, sig)] -- Direct name lookup
        ([], [], Nothing) -> []
  in case candidates of
    [] -> Left $ mkError ("function '" <> fname <> "' to exist") "undefined function"
    [(name, (_, args))] ->
      case checkEachParam s file line col 0 es args of
        Nothing -> Right name
        Just err -> Left err
    multiples ->
      -- Filter to only those with matching parameter count and compatible types
      case filter (\(_, (_, args)) -> checkEachParam s file line col 0 es args == Nothing) multiples of
        [(name, _)] -> Right name
        [] -> Left $ mkError ("function '" <> fname <> "' with compatible arguments") "no matching signature"
        _ -> Left $ mkError (printf "multiple signatures for %s" fname) "ambiguous function call"

-- | Check if a function name looks like a struct method (StructName_methodName)
isStructMethod :: String -> Bool
isStructMethod name = '_' `elem` name

-- | Check if a function name is a valid mangling of the base name with compatible types
isCompatibleMangling :: String -> String -> Type -> [Type] -> [Type] -> Bool
isCompatibleMangling baseName fName retType funcArgTypes argTypes =
  let -- The mangled name should be retType_baseName_... 
      expectedPrefix = show retType <> "_" <> baseName
      isNameMatch = fName == baseName ||
                    fName == expectedPrefix ||
                    (expectedPrefix <> "_") `isPrefixOfStr` fName
      argsCompatible = length argTypes == length funcArgTypes &&
                       all (uncurry isTypeCompatible) (zip funcArgTypes argTypes)
  in isNameMatch && argsCompatible
  where
    isPrefixOfStr prefix str = take (length prefix) str == prefix

isRightFunction :: (String, [Type]) -> String -> (Type, [Type]) -> Bool
isRightFunction (funcToCheck, argTypesToCheck) fname (retType, _) =
  fname == mangleName funcToCheck retType argTypesToCheck

mangleName :: String -> Type -> [Type] -> String
mangleName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = intercalate "_" (show ret : fname : map show args)


exprType :: Stack -> Expression -> Either String Type
exprType _ (ExprLitInt _ _)         = Right TypeI32
exprType _ (ExprLitFloat _ _)       = Right TypeF32
exprType _ (ExprLitString _ _)      = Right TypeString
exprType _ (ExprLitChar _ _)        = Right TypeChar
exprType _ (ExprLitBool _ _)        = Right TypeBool
exprType _ (ExprStructInit _ st _)  = Right $ TypeCustom st
exprType _ (ExprLitNull _)          = Right TypeNull
exprType s (ExprAccess pos target field) = do
  targetType <- exprType s target
  let ss = case s of (_, _, ss') -> ss'
  case getFieldType pos ss targetType field of
    Right t -> Right t
    Left err -> Left (formatSemanticError err)

exprType _ (ExprCast _ _ t)         = Right t

exprType s (ExprBinary _ op a b)    = do 
  a' <- exprType s a
  b' <- exprType s b
  iHTBinary op a' b'

exprType s (ExprUnary _ _ expr)     = exprType s expr
exprType (_, vs, _) (ExprVar _ name) = Right $ fromMaybe TypeAny (HM.lookup name vs)

exprType s@(fs, _, _) (ExprCall _ (ExprVar _ fn) args) = do
  argTypes <- mapM (exprType s) args
  Right $ fromMaybe TypeAny (selectSignature fs fn argTypes)

exprType _ (ExprCall _ _ _) = Right TypeAny

exprType s (ExprIndex _ target _) = exprType s target >>= extractArrayType
  where
    extractArrayType (TypeArray inner) = Right inner
    extractArrayType TypeString = Right TypeChar
    extractArrayType TypeAny = Right TypeAny
    extractArrayType (TypePtr inner) = Right inner
    extractArrayType t = Left $ printf "\n\tIndexingNonArray: cannot index type %s, expected array" (show t)

exprType _ (ExprLitArray _ []) = Right $ TypeArray TypeAny
exprType s (ExprLitArray _ (e:es)) = do
  firstType <- exprType s e
  allTypes <- mapM (exprType s) es
  checkArray firstType allTypes
  where
    checkArray t ts
      | all (isTypeCompatible t) ts = Right $ TypeArray t
      | otherwise = Left $ printf "\n\tIncompatibleArrayElements: array elements have incompatible types, first is %s" (show t)


assignVarType :: VarStack -> String -> String -> Int -> Int -> Type -> Either SemanticError VarStack
assignVarType vs _ _ _ _ TypeAny = Right vs
assignVarType vs v file line col t =
  let updated = Right $ HM.insert v t vs
  in case HM.lookup v vs of
    Nothing       -> updated
    Just TypeAny  -> updated
    Just TypeNull -> updated
    Just t' | isTypeCompatible t t' -> updated
            | otherwise -> Left $ SemanticError file line col (printf "variable '%s' to have type %s" v (show t')) (printf "type %s" (show t)) ["variable assignment", "global context"]


isTypeCompatible :: Type -> Type -> Bool
isTypeCompatible TypeAny _ = True
isTypeCompatible _ TypeAny = True
isTypeCompatible (TypePtr TypeAny) _ = True  -- *any accepts any type
isTypeCompatible _ (TypePtr TypeAny) = True  -- anything can be passed as *any
isTypeCompatible (TypePtr _) TypeNull = True
isTypeCompatible TypeNull (TypePtr _) = True
isTypeCompatible (TypePtr TypeChar) TypeString = True
isTypeCompatible TypeString (TypePtr TypeChar) = True
isTypeCompatible (TypePtr a) (TypePtr b) = isTypeCompatible a b
isTypeCompatible expected actual
  | sameType expected actual = True
  | actual == TypeI32 && isIntegerType expected = True
  | actual == TypeF32 && isFloatType expected   = True
  | isIntegerType expected && isIntegerType actual = True
  | isFloatType expected && isFloatType actual = True
  | otherwise = False


checkMultipleType :: String -> String -> Int -> Int -> Maybe Type -> Type -> Either SemanticError Type
checkMultipleType _ _ _ _ Nothing e_t         = Right e_t
checkMultipleType _ _ _ _ (Just TypeAny) e_t  = Right e_t
checkMultipleType _ _ _ _ (Just (TypeArray TypeAny)) (TypeArray inner) = Right (TypeArray inner)
checkMultipleType _ _ _ _ (Just t) TypeAny    = Right t
checkMultipleType _ _ _ _ _ TypeNull          = Right TypeNull
checkMultipleType v file line col (Just t) e_t
  | isTypeCompatible t e_t = Right t
  | otherwise = Left $ SemanticError file line col (printf "variable '%s' to have type %s" v (show t)) (printf "type %s being assigned" (show e_t)) ["type check", "global context"]


checkEachParam :: Stack -> String -> Int -> Int -> Int -> [Expression] -> [Type] -> Maybe SemanticError
checkEachParam s file line col i (e:es) (t:at) =
  case exprType s e of
    Left err -> Just $ SemanticError file line col "valid expression type" err ["parameter check", "function call"]
    Right t' ->
      if isTypeCompatible t t'
      then checkEachParam s file line col (i + 1) es at
      else 
        let expected = printf "argument %d to have type %s" i (show t)
            got = printf "type %s" (show t')
        in Just $ SemanticError file line col expected got ["parameter check", "function call", "global context"]

checkEachParam _ _ _ _ _ [] [] = Nothing
checkEachParam _ file line col i [] at =
  let expected = printf "%d arguments" (length at + i)
      got = printf "%d arguments (too few)" i
  in Just $ SemanticError file line col expected got ["parameter count", "function call", "global context"]
checkEachParam _ file line col i es [] =
  let expected = printf "%d arguments" i
      got = printf "%d arguments (too many)" (length es + i)
  in Just $ SemanticError file line col expected got ["parameter count", "function call", "global context"]


selectSignature :: FuncStack -> String -> [Type] -> Maybe Type
selectSignature fs name argTypes =
  let mangled = HM.filterWithKey (\k (ret, args) -> isRightFunction (name, argTypes) k (ret, args)) fs
  in case (HM.toList mangled, HM.lookup name fs) of
    ((_, (ret, _)):_, _) -> Just ret          -- Trouvé manglé
    ([], Just (ret, _))  -> Just ret          -- Trouvé en base
    _                    -> Nothing            -- Pas trouvé

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
