module Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , selectSignature
  , checkEachParam
  ) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.OpType (iHTBinary, sameType)

import Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , Stack
  )

checkParamType :: Stack -> String -> [Expression] -> Either String String
checkParamType s@(fs, _) fname es =
  let unknown_func = "\n\tUnknownFunction: %s is not known"
      no_match = "\n\tNoMatchingSignature: %s doesn't have any signature like this %s"
  in case HM.lookup fname fs of
    Nothing         -> Left $ printf unknown_func fname
    Just []         -> Left $ printf unknown_func fname
    Just [sig]      -> checkSingle sig
    Just (sig:sigs) -> case checkSingle sig of
                         Left _  -> checkAll (printf no_match fname (show (sig:sigs))) sigs
                         Right r -> Right r
  where
    -- if there is 1 signature so no override
    checkSingle (_, at) =
      case checkEachParam s 0 es at of
        Nothing  -> Right fname
        Just err -> Left err

    -- if multiple signature so mangle name
    checkAll err_msg [] = Left err_msg
    checkAll err_msg ((ret, at):rest) =
      case checkEachParam s 0 es at of
        Nothing  -> Right $ mangleName fname ret at
        Just _   -> checkAll err_msg rest

mangleName :: String -> Type -> [Type] -> String
mangleName fname ret args =
    intercalate "_" (show ret : fname : map show args)

exprType :: Stack -> Expression -> Either String Type
exprType _ (ExprLitInt _)         = Right TypeI32
exprType _ (ExprLitFloat  _)      = Right TypeF32
exprType _ (ExprLitString  _)     = Right TypeString
exprType _ (ExprLitChar _ )       = Right TypeU8
exprType _ (ExprLitBool  _)       = Right TypeBool
exprType _ (ExprStructInit st _)  = Right $ TypeCustom st
exprType _ ExprLitNull            = Right TypeNull
exprType _ (ExprAccess _ _)       = Right TypeAny -- don't know how to use struct
exprType s (ExprBinary op a b)    = do 
  a' <- exprType s a
  b' <- exprType s b
  iHTBinary op a' b'
exprType s (ExprUnary _ expr)     = exprType s expr -- assume the op don't change the type
exprType (_, vs) (ExprVar name)   = Right $ fromMaybe TypeAny (HM.lookup name vs)
exprType s@(fs, _) (ExprCall fn args) = do
  argTypes <- sequence $ map (exprType s) args
  Right $ fromMaybe TypeAny (selectSignature fs fn argTypes)

assignVarType :: VarStack -> String -> Type -> Either String VarStack
assignVarType vs _ TypeAny = Right vs
assignVarType vs v t =
  let msg = "\n\tTypeOverwrite: %s has already %s but %s were given"
      updated = Right $ HM.insert v t vs
  in case HM.lookup v vs of
    Nothing       -> updated
    Just TypeAny  -> updated
    Just TypeNull -> updated
    Just t' | sameType t t' -> updated
            | otherwise     -> Left $ printf msg v (show t') (show t)

checkMultipleType :: String -> Maybe Type -> Type -> Either String Type
checkMultipleType _ Nothing e_t         = Right e_t
checkMultipleType _ (Just TypeAny) e_t  = Right e_t
checkMultipleType _ (Just t) TypeAny    = Right t
checkMultipleType _ _ TypeNull          = Right TypeNull
checkMultipleType v (Just t) e_t
  | sameType t e_t  = Right t
  | otherwise =
    let msg = "\n\tMultipleType: %s is already %s and %s is trying to be assigned"
    in Left $ printf msg v (show t) (show e_t)

checkEachParam :: Stack -> Int -> [Expression] -> [Type] -> Maybe String
checkEachParam s i (_:es) (TypeAny:at) = checkEachParam s (i + 1) es at
checkEachParam s i (e:es) (t:at) =
  let wrong_type = "\n\tWrongType: arg%d exp %s but have %s"
      next = checkEachParam s (i + 1) es at
  in case exprType s e of
    Left err -> Just err
    Right t' | sameType t t'  -> next
             | otherwise      -> Just (printf wrong_type i (show t) (show t')) <> next 
checkEachParam _ _ [] [] = Nothing
checkEachParam _ i [] at = Just $ printf ("\n\tWrongNbArgs: exp %d but %d were given (too few)") (length at + i) (i)
checkEachParam _ i es [] = Just $ printf ("\n\tWrongNbArgs: exp %d but %d were given (too many)") (i) (length es + i)

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


