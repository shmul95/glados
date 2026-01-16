{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Func
  ( findFunc
  , findDefs
  , transformStructMethods
  , mangleFuncName
  , inferParamType
  )
where
#else
module Rune.Semantics.Func (findFunc, inferParamType) where
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
findFunc (Program _ defs) = foldM findDefs HM.empty defs

--
-- private
--

findDefs :: FuncStack -> TopLevelDef -> Either String FuncStack

findDefs s (DefFunction name params rType _ _) =
    case HM.lookup finalName s of
        Nothing       -> Right $ HM.insert finalName sig s
        Just existing -> handleConflict existing
  where
    sig         = (rType, params)
    pTypes      = map paramType params
    mangledName = mangleFuncName name rType pTypes
    finalName   = if name == "main" then name else mangledName

    handleConflict (exRet, exArgs)
        | exRet == rType && exArgs == params = Left errSameSig
        | otherwise                          = Left errMangled

    errSameSig = printf "FuncAlreadyExist: %s was already defined with same signature" name
    errMangled = printf "FuncAlreadyExist: %s (mangled: %s) was already defined" name finalName

findDefs s (DefSomewhere sigs) = foldM addSig s sigs
  where
    addSig fs (FunctionSignature name pTypes rType isExtern) =
        Right $ HM.insert finalName sig fs
      where
        sig       = (rType, params)
        params    = map (\t -> Parameter "" t Nothing) pTypes
        finalName = if isExtern then name else mangleFuncName name rType pTypes

findDefs s (DefStruct name _ methods) = foldM findDefs s $ transformStructMethods name methods

-- | Infer parameter type from default value if type is TypeAny
inferParamType :: Parameter -> Parameter
inferParamType (Parameter name TypeAny (Just defaultExpr)) =
  Parameter name (inferTypeFromExpr defaultExpr) (Just defaultExpr)
inferParamType param = param

-- | Infer type from a literal expression
inferTypeFromExpr :: Expression -> Type
inferTypeFromExpr (ExprLitInt _ _) = TypeI32
inferTypeFromExpr (ExprLitFloat _ _) = TypeF32
inferTypeFromExpr (ExprLitString _ _) = TypeString
inferTypeFromExpr (ExprLitChar _ _) = TypeChar
inferTypeFromExpr (ExprLitBool _ _) = TypeBool
inferTypeFromExpr (ExprLitNull _) = TypeNull
inferTypeFromExpr (ExprLitArray _ []) = TypeArray TypeAny
inferTypeFromExpr (ExprLitArray _ (e:_)) = TypeArray (inferTypeFromExpr e)
inferTypeFromExpr (ExprStructInit _ sName _) = TypeCustom sName
inferTypeFromExpr _ = TypeAny  -- For complex expressions, keep TypeAny

-- | check if a method is static (doesn't need self)
-- TODO: add maybe more static method such as static keyword idk
isStaticMethod :: String -> Bool
isStaticMethod "new" = True
isStaticMethod _     = False

transformStructMethods :: String -> [TopLevelDef] -> [TopLevelDef]
transformStructMethods sName = map transform
  where
    transform (DefFunction methodName params rType body isExport) =
      let baseName = sName ++ "_" ++ methodName
          -- paramsInferred = map inferParamType params
          -- params' = if isStaticMethod methodName then paramsInferred else fixSelfType sName paramsInferred
          params' = if isStaticMethod methodName then params else fixSelfType sName params
      in DefFunction baseName params' rType body isExport
    transform other = other

mangleFuncName :: String -> Type -> [Type] -> String
mangleFuncName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = List.intercalate "_" (show ret : fname : map show args)
