{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Func
  ( findFunc
  , findDefs
  , transformStructMethods
  , mangleFuncName
  , inferParamType
  , inferTypeFromExpr
  )
where
#else
module Rune.Semantics.Func
  ( findFunc
  , inferParamType
  , inferTypeFromExpr
  )
where
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

findDefs s (DefFunction name params rType _ _ visibility isStatic _) =
    case HM.lookup name s of
        Nothing       -> Right $ HM.insert name sig s
        Just (existing, _, _) -> handleConflict existing
  where
    sig         = ((rType, params), visibility, isStatic)
    pTypes      = map paramType params
    mangledName = mangleFuncName name rType pTypes

    handleConflict (exRet, exArgs)
        | exRet == rType && exArgs == params = Left errSameSig
        | HM.member mangledName s            = Left errMangled
        | otherwise                          = Right $ HM.insert mangledName sig s

    errSameSig = printf "FuncAlreadyExist: %s was already defined with same signature" name
    errMangled = printf "FuncAlreadyExist: %s (mangled: %s) was already defined" name mangledName

findDefs s (DefSomewhere sigs) = foldM addSig s sigs
  where
    addSig fs (FunctionSignature name pTypes rType isExtern) =
        Right $ HM.insert finalName sig fs
      where
        sig       = ((rType, params), Public, False)
        params    = map (\t -> Parameter "" t Nothing) pTypes
        finalName = if isExtern then name else resolveName

        resolveName = if HM.member name fs 
                      then mangleFuncName name rType pTypes 
                      else name

findDefs s (DefStruct name _ methods _ _) = foldM findDefs s $ transformStructMethods name methods

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
-- isStaticMethod :: String -> Bool
-- isStaticMethod "new" = True
-- isStaticMethod _     = False

transformStructMethods :: String -> [TopLevelDef] -> [TopLevelDef]
transformStructMethods sName = map transform
  where
    transform (DefFunction methodName params rType body isExport visibility isStatic isAbstract) =
      let baseName = sName ++ "_" ++ methodName
          -- paramsInferred = map inferParamType params
          -- params' = if isStaticMethod methodName then paramsInferred else fixSelfType sName paramsInferred
          params' = if isStatic then params else fixSelfType sName params
      in DefFunction baseName params' rType body isExport visibility isStatic isAbstract
    transform other = other

mangleFuncName :: String -> Type -> [Type] -> String
mangleFuncName fname ret args
  | TypeAny `elem` args || ret == TypeAny = fname
  | otherwise = List.intercalate "_" (show ret : fname : map show args)