{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Func
  ( findFunc
  , findDefs
  , transformStructMethods
  , mangleFuncName
  , hasDuplicate
  , findDuplicateMap
  )
where
#else
module Rune.Semantics.Func (findFunc) where
#endif

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
        msg = "FuncAlreadyExist: %s was already defined, use override"
    in case HM.lookup name s of
      Just _ -> Left $ printf msg name
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
        mangledName = mangleFuncName name rType paramTypes
    in case HM.lookup name s of
      Just list -> 
        let s' = HM.insert name (list <> [newSign]) s
        in if mangledName == name
           then Right s'
           else Right $ HM.insert mangledName [newSign] s'
      Nothing   -> Left $ printf msg name

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

-- | find struct method definitions
-- check for duplicate method names in the struct
-- transform method names to include struct name as prefix
-- then process methods as normal function definitions
findDefs s (DefStruct name _ methods) =
    let defFuncNames = [methodName | DefFunction methodName _ _ _ _ <- methods]
        funcDuplicates = defFuncNames List.\\ List.nub defFuncNames
        msg = "DuplicateMethodInStruct: Duplicate method '%s' in struct '%s' (use override for additional signatures)"
    in case funcDuplicates of
      (dup:_) -> Left $ printf msg dup name
      [] -> foldM findDefs s (transformStructMethods name methods)

transformStructMethods :: String -> [TopLevelDef] -> [TopLevelDef]
transformStructMethods sName = map transform
  where
    transform (DefFunction methodName params rType body isExport) =
      let baseName = sName ++ "_" ++ methodName
          params' = fixSelfType sName params
      in DefFunction baseName params' rType body isExport
    transform (DefOverride methodName params rType body isExport) =
      let baseName = sName ++ "_" ++ methodName
          params' = fixSelfType sName params
      in DefOverride baseName params' rType body isExport
    transform other = other

mangleFuncName :: String -> Type -> [Type] -> String
mangleFuncName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = List.intercalate "_" (show ret : fname : map show args)

hasDuplicate :: (Ord a) => [a] -> Bool
hasDuplicate xs = Set.size (Set.fromList xs) /= length xs

findDuplicateMap :: FuncStack -> String -> Maybe String
findDuplicateMap fs msg = foldr check Nothing (HM.toList fs)
  where 
    check _ (Just err) = Just err
    check (name, sigs) Nothing
      | hasDuplicate sigs = Just $ printf msg name (show sigs)
      | otherwise = Nothing

