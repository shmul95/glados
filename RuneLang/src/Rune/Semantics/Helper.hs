module Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , selectSignature
  , checkEachParam
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
checkParamType s@(fs, _) fname file line col es =
  let mkError expected got = SemanticError file line col expected got ["function call", "global context"]
  in case HM.lookup fname fs of
    Nothing         -> Left $ mkError ("function '" ++ fname ++ "' to exist") "undefined function"
    Just []         -> Left $ mkError ("function '" ++ fname ++ "' to exist") "undefined function"
    Just [sig]      -> checkSingle sig
    Just (sig:sigs) -> case checkSingle sig of
                         Left _ -> checkAll (mkError (printf "matching signature for %s with arguments %s" fname (show $ map (exprType s) es)) "no matching overload") sigs
                         Right r  -> Right r
  where
    -- if there is 1 signature so no override
    checkSingle (_, at) =
      case checkEachParam s file line col 0 es at of
        Nothing  -> Right fname
        Just err -> Left err

    -- if multiple signature so mangle name
    checkAll err_msg [] = Left err_msg
    checkAll err_msg ((ret, at):rest) =
      case checkEachParam s file line col 0 es at of
        Nothing  -> Right $ mangleName fname ret at
        Just _   -> checkAll err_msg rest

mangleName :: String -> Type -> [Type] -> String
mangleName fname ret args =
    intercalate "_" (show ret : fname : map show args)

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
exprType _ (ExprAccess _ _ _) = TypeAny -- don't know how to use struct
exprType s (ExprBinary _ _ expr _) = exprType s expr -- assume both expr are of the same type
exprType s (ExprUnary _ _ expr) = exprType s expr -- assume the op don't change the type
exprType (_, vs) (ExprVar _ name) = fromMaybe TypeAny (HM.lookup name vs)
exprType s@(fs, _) (ExprCall _ fn args) =
  let argTypes = map (exprType s) args
  in case selectSignature fs fn argTypes of
    Just ret -> ret
    Nothing  -> TypeAny

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
  | otherwise = Left $ SemanticError file line col (printf "variable '%s' to have type %s" v (show t)) (printf "type %s being assigned" (show e_t)) ["type check", "global context"]

checkEachParam :: Stack -> String -> Int -> Int -> Int -> [Expression] -> [Type] -> Maybe SemanticError
checkEachParam _ _ _ _ _ [] [] = Nothing
checkEachParam s file line col i (_:es) (TypeAny:at) = checkEachParam s file line col (i + 1) es at
checkEachParam s file line col i (e:es) (t:at) =
  let expected = printf "argument %d to have type %s" i (show t)
      got = printf "type %s" (show $ exprType s e)
  in if exprType s e == t
     then checkEachParam s file line col (i + 1) es at
     else Just $ SemanticError file line col expected got ["parameter check", "function call", "global context"]
checkEachParam _ file line col i [] at =
  let expected = printf "%d arguments" (length at + i)
      got = printf "%d arguments (too few)" i
  in Just $ SemanticError file line col expected got ["parameter count", "function call", "global context"]
checkEachParam _ file line col i es [] =
  let expected = printf "%d arguments" i
      got = printf "%d arguments (too many)" (length es + i)
  in Just $ SemanticError file line col expected got ["parameter count", "function call", "global context"]


