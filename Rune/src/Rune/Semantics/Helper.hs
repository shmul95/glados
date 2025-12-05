module Rune.Semantics.Helper
  ( checkParamType
  , exprType
  , assignVarType
  , checkMultipleType
  , selectSignature
  , checkEachParam
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM

import Text.Printf (printf)

import Rune.AST.Nodes

import Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , Stack
  )

checkParamType :: Stack -> String -> [Expression] -> Maybe String
checkParamType (fs, vs) fname es =
  let unknown_func = "\n\tUnknownFunction: %s is not known"
      no_match = "\n\tNoMatchingSignature: %s doesn't have any signature like this %s"
  in case HM.lookup fname fs of
    Nothing -> Just $ printf unknown_func fname
    Just [] -> Just $ printf unknown_func fname
    Just sigs -> tryAll (printf no_match fname (show sigs)) sigs
  where
    tryAll err_msg [] = Just err_msg
    tryAll err_msg ((_, at):rest) =
      case checkEachParam (fs, vs) 0 es at of
        Nothing -> Nothing
        Just _  -> tryAll err_msg rest

exprType :: Stack -> Expression -> Type
exprType _ (ExprLitInt _) = TypeI32
exprType _ (ExprLitFloat  _) = TypeF32
exprType _ (ExprLitString  _) = TypeString
exprType _ (ExprLitChar _ ) = TypeU8
exprType _ (ExprLitBool  _) = TypeBool
exprType _ (ExprStructInit st _) = TypeCustom st
exprType _ ExprLitNull = TypeNull
exprType _ (ExprAccess _ _) = TypeAny -- don't know how to use struct
exprType s (ExprBinary _ expr _) = exprType s expr -- assume both expr are of the same type
exprType s (ExprUnary _ expr) = exprType s expr -- assume the op don't change the type
exprType (_, vs) (ExprVar name) = fromMaybe TypeAny (HM.lookup name vs)
exprType s@(fs, _) (ExprCall fn args) =
  let argTypes = map (exprType s) args
  in case selectSignature fs fn argTypes of 
    Just ret -> ret
    Nothing  -> TypeAny


assignVarType :: VarStack -> String -> Type -> (VarStack, Maybe String)
assignVarType s _ TypeAny = (s, Nothing)
assignVarType s v t =
    let msg = "\n\tTypeOverwrite: %s has already %s but %s were given"
        updated = (HM.insert v t s, Nothing)
    in case HM.lookup v s of
      Nothing       -> updated
      Just TypeAny  -> updated
      Just TypeNull -> updated
      Just t' | t' == t   -> updated
              | otherwise -> (s, Just $ printf msg v (show t') (show t))

checkMultipleType :: String -> Maybe Type -> Type -> Maybe String
checkMultipleType _ Nothing _      = Nothing
checkMultipleType _ (Just TypeAny) _ = Nothing
checkMultipleType _ _ TypeAny      = Nothing
checkMultipleType _ _ TypeNull     = Nothing
checkMultipleType v (Just t) e_t
  | t == e_t  = Nothing
  | otherwise =
    let msg = "\n\tMultipleType: %s is already %s and %s is trying to be assigned"
    in Just $ printf msg v (show t) (show e_t)

checkEachParam :: Stack -> Int -> [Expression] -> [Type] -> Maybe String
checkEachParam s i (_:es) (TypeAny:at) = checkEachParam s (i + 1) es at
checkEachParam s i (e:es) (t:at) =
  let wrong_type = "\n\tWrongType: arg%d exp %s but have %s"
      next = checkEachParam s (i + 1) es at
  in case exprType s e == t of
    True  -> next
    False -> Just (printf wrong_type i (show t) (show $ exprType s e)) <> next
checkEachParam _ _ [] [] = Nothing
checkEachParam _ i [] at = Just $ printf ("\n\tWrongNbArgs: exp %d but %d where given (too less)") (length at + i) (i)
checkEachParam _ i es [] = Just $ printf ("\n\tWrongNbArgs: exp %d but %d where given (too much)") (i) (length es + i)

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


