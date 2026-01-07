module Rune.Semantics.Func (findFunc) where

import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.List as List

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Type (FuncStack)
import Rune.Semantics.Helper (fixSelfType)

--
-- public
--

findFunc :: Program -> Either String FuncStack
findFunc (Program _ defs) = do
  let builtins = HM.fromList
        [ ("show" , [(TypeNull, [TypeAny])])
        , ("error", [(TypeNull, [TypeAny])])
        ]
      msg = "\n\tHasDuplicates: %s has duplicate signatures (%s)"
  fs <- foldM findDefs builtins defs
  maybe (Right fs) Left (findDuplicateMap fs msg)

--
-- private
--

findDefs :: FuncStack -> TopLevelDef -> Either String FuncStack

-- | find normal function definitions
-- lookup for existing function name in the stack
-- if found, check if the signature already exists
-- if the signature exists -> error
-- if not found, insert the new function signature
findDefs s (DefFunction name params rType _ _) =
    let paramTypes = map paramType params
        newSign = (rType, paramTypes)
        msg = "\n\tFuncAlreadyExist: %s was already defined, use override"
    in case HM.lookup name s of
      Just existing -> 
        if newSign `elem` existing
          then Right s
          else Left $ printf msg name
      Nothing -> Right $ HM.insert name [newSign] s

-- | find override function definitions
-- lookup for existing function name in the stack
-- if found, append the new signature
-- if the mangled name differs, insert the mangled version as well
-- if not found, -> invalid override error
findDefs s (DefOverride name params rType _ _) =
    let paramTypes = map paramType params
        newSign = (rType, paramTypes)
        msg = "\n\tWrongOverrideDef: %s is declared as override without any base function"
    in case HM.lookup name s of
      Just list ->
        let (generic, specific) = List.partition (\(_, args) -> TypeAny `elem` args) list
        in Right $ HM.insert name (specific ++ [newSign] ++ generic) s
      Nothing   -> Left $ printf msg name
findDefs s (DefStruct name _ methods) =
    foldM addMethod s methods
  where
    addMethod acc (DefFunction methodName params rType _ _) =
      let baseName = name ++ "_" ++ methodName
          params' = fixSelfType name params
          paramTypes = map paramType params'
          newSign = (rType, paramTypes)
      in Right $ HM.insertWith (++) baseName [newSign] acc
    addMethod acc _ = Right acc

-- | find function signatures defined somewhere else
-- iterate over all signatures
-- if override, always insert or append the signature
-- if not override, insert only if the function does not already exist
findDefs s (DefSomewhere sigs) = foldM addSig s sigs
  where
    addSig fs (FunctionSignature name paramTypes rType isOverride) =
      let newSign = (rType, paramTypes)
      in if isOverride
         then case HM.lookup name fs of
                Just list -> Right $ HM.insert name (list <> [newSign]) fs
                Nothing   -> Right $ HM.insert name [newSign] fs
         else case HM.lookup name fs of
                Nothing -> Right $ HM.insert name [newSign] fs
                Just _  -> Right fs

hasDuplicate :: (Ord a) => [a] -> Bool
hasDuplicate xs = Set.size (Set.fromList xs) /= length xs

findDuplicateMap :: FuncStack -> String -> Maybe String
findDuplicateMap fs msg = foldr check Nothing (HM.toList fs)
  where
    check _ (Just err) = Just err
    check (name, sigs) Nothing
      | hasDuplicate sigs = Just $ printf msg name (show sigs)
      | otherwise = Nothing
