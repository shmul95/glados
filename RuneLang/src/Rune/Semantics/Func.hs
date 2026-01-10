{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Func
  ( findFunc
  , findDefs
  , transformStructMethods
  , mangleFuncName
  )
where
#else
module Rune.Semantics.Func (findFunc) where
#endif

import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
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
        [ ("show" , (TypeNull, [TypeAny]))
        , ("error", (TypeNull, [TypeAny]))
        ]
  foldM findDefs builtins defs

--
-- private
--

findDefs :: FuncStack -> TopLevelDef -> Either String FuncStack

-- | find normal function definitions
findDefs s (DefFunction name params rType _ _) =
    let paramTypes = map paramType params
        sig = (rType, paramTypes)
        mangledName = mangleFuncName name rType paramTypes
    in if HM.member mangledName s
       then Left $ printf "FuncAlreadyExist: %s (signature: %s) was already defined" name (show sig)
       else Right $ HM.insert mangledName sig s

-- | find override function definitions
findDefs s (DefOverride name params rType _ _) =
    let paramTypes = map paramType params
        sig = (rType, paramTypes)
        mangledName = mangleFuncName name rType paramTypes
        msg = "\n\tWrongOverrideDef: %s is declared as override without any base function"
    in if HM.member name s
       then Right $ HM.insert mangledName sig s
       else Left $ printf msg name

-- | find function signatures defined somewhere else
findDefs s (DefSomewhere sigs) = foldM addSig s sigs
  where
    addSig fs (FunctionSignature name paramTypes rType _isOverride) =
      let sig = (rType, paramTypes)
          mangledName = mangleFuncName name rType paramTypes
      in Right $ HM.insertWith (\_ old -> old) mangledName sig fs

-- | find struct method definitions
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
