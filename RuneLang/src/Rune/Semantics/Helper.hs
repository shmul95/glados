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
  ) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM

import Text.Printf (printf)

import Rune.AST.Nodes

import Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , Stack
  )
import Rune.Semantics.OpType (isIntegerType, isFloatType, iHTBinary, sameType)

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


checkParamType :: Stack -> String -> String -> Int -> Int -> [Expression] -> Either SemanticError String
checkParamType s@(fs, _) fname file line col es =
  let mkError expected got = SemanticError file line col expected got ["function call", "global context"]
  in case HM.lookup fname fs of
    Nothing         -> Left $ mkError ("function '" <> fname <> "' to exist") "undefined function"
    Just []         -> Left $ mkError ("function '" <> fname <> "' to exist") "undefined function"
    Just [sig]      -> checkSingle sig
    Just (sig:sigs) -> case checkSingle sig of
                         Left _ -> checkAll (mkError (printf "matching signature for %s" fname) "no matching overload") sigs
                         Right r  -> Right r
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


exprType :: Stack -> Expression -> Either String Type
exprType _ (ExprLitInt _ _)         = Right TypeI32
exprType _ (ExprLitFloat _ _)       = Right TypeF32
exprType _ (ExprLitString _ _)      = Right TypeString
exprType _ (ExprLitChar _ _)        = Right TypeChar
exprType _ (ExprLitBool _ _)        = Right TypeBool
exprType _ (ExprStructInit _ st _)  = Right $ TypeCustom st
exprType _ (ExprLitNull _)          = Right TypeNull
exprType _ (ExprAccess {})       = Right TypeAny -- don't know how to use struct
exprType _ (ExprCast _ _ t)         = Right t

exprType s (ExprBinary _ op a b)    = do 
  a' <- exprType s a
  b' <- exprType s b
  iHTBinary op a' b'

exprType s (ExprUnary _ _ expr)     = exprType s expr -- assume the op don't change the type
exprType (_, vs) (ExprVar _ name)   = Right $ fromMaybe TypeAny (HM.lookup name vs)

exprType s@(fs, _) (ExprCall _ fn args) = do
  argTypes <- mapM (exprType s) args
  Right $ fromMaybe TypeAny (selectSignature fs fn argTypes)

exprType s (ExprIndex _ target _) = exprType s target >>= extractArrayType
  where
    extractArrayType (TypeArray inner) = Right inner
    extractArrayType TypeString = Right TypeChar
    extractArrayType TypeAny = Right TypeAny
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
isTypeCompatible (TypePtr _) TypeNull = True
isTypeCompatible TypeNull (TypePtr _) = True
isTypeCompatible (TypePtr TypeAny) (TypePtr _) = True
isTypeCompatible (TypePtr _) (TypePtr TypeAny) = True
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
selectSignature fs name at =
  case HM.lookup name fs of
    Nothing   -> Nothing
    Just []   -> Nothing
    Just sigs ->
      case filter (match at) sigs of
        [(rt, _)] -> Just rt
        matches@(_:_) -> Just . fst $ mostSpecific matches
        _         -> Nothing
  where
    match :: [Type] -> (Type, [Type]) -> Bool
    match act (_, expec) =
      length act == length expec &&
      and (zipWith isTypeCompatible expec act)

    mostSpecific :: [(Type, [Type])] -> (Type, [Type])
    mostSpecific = foldr1 moreSpecific
    
    moreSpecific :: (Type, [Type]) -> (Type, [Type]) -> (Type, [Type])
    moreSpecific s1@(_, params1) s2@(_, params2)
      | specificity params1 > specificity params2 = s1
      | otherwise = s2
    
    specificity :: [Type] -> Int
    specificity = sum . map typeSpecificity
    
    typeSpecificity :: Type -> Int
    typeSpecificity TypeAny = 0
    typeSpecificity (TypeArray TypeAny) = 1
    typeSpecificity (TypeArray t) = 2 + typeSpecificity t
    typeSpecificity _ = 3
