module Rune.Semantics.Func (findFunc) where

import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Type (FuncStack)

findFunc :: Program -> Either String FuncStack
findFunc (Program _ defs) = do
  let builtins = HM.fromList
        [ ("show" , [(TypeNull, [TypeAny])])
        , ("error", [(TypeNull, [TypeAny])])
        ]
      msg = "\n\tHasDuplicates: %s has duplicate signatures (%s)"
  fs <- foldM findDefs builtins defs
  maybe (Right fs) Left (findDuplicateMap fs msg)

findDefs :: FuncStack -> TopLevelDef -> Either String FuncStack
findDefs s (DefFunction name params rType _) =
    let paramTypes = map paramType params
        newSign = (rType, paramTypes)
        msg = "\n\tFuncAlreadyExist: %s was already defined, use override"
    in case HM.lookup name s of
      Nothing -> Right $ HM.insert name [newSign] s
      Just _  -> Left $ printf msg name
findDefs s (DefOverride name params rType _) =
    let paramTypes = map paramType params
        newSign = (rType, paramTypes)
        msg = "\n\tWrongOverrideDef: %s is declared as override without any base function"
    in case HM.lookup name s of
      Just list -> Right $ HM.insert name (list ++ [newSign]) s
      Nothing   -> Left $ printf msg name
findDefs s _ = Right s

hasDuplicate :: (Ord a) => [a] -> Bool
hasDuplicate xs = Set.size (Set.fromList xs) /= length xs

findDuplicateMap :: FuncStack -> String -> Maybe String
findDuplicateMap fs msg = foldr check Nothing (HM.toList fs)
  where 
    check _ (Just err) = Just err
    check (name, sigs) Nothing
      | hasDuplicate sigs = Just $ printf msg name (show sigs)
      | otherwise = Nothing

