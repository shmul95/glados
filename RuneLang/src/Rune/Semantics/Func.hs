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
findFunc (Program _ defs) = do
  let builtins = HM.fromList
        [ ("show" , (TypeNull, [Parameter "value" TypeAny Nothing]))
        , ("error", (TypeNull, [Parameter "msg" TypeAny Nothing]))
        ]
  foldM findDefs builtins defs

--
-- private
--

findDefs :: FuncStack -> TopLevelDef -> Either String FuncStack

-- | find function definitions
findDefs s (DefFunction name params rType _ _) =
    let paramTypes = map paramType params
        sig = (rType, params)
    in case HM.lookup name s of
         Nothing -> Right $ HM.insert name sig s
         Just (existingRet, existingArgs) ->
             if existingRet == rType && existingArgs == params
             then Left $ printf "FuncAlreadyExist: %s was already defined with same signature" name
             else
                 let mangledName = mangleFuncName name rType paramTypes
                 in if HM.member mangledName s
                    then Left $ printf "FuncAlreadyExist: %s (mangled: %s) was already defined" name mangledName
                    else Right $ HM.insert mangledName sig s

-- | find function signatures defined somewhere else
findDefs s (DefSomewhere sigs) = foldM addSig s sigs
  where
    addSig fs (FunctionSignature name paramTypes rType) =
      let params = map (\pType -> Parameter "" pType Nothing) paramTypes
          sig = (rType, params)
      in Right $ HM.insertWith (\_ old -> old) name sig fs

-- | find struct method definitions
findDefs s (DefStruct name _ methods) =
    foldM findDefs s (transformStructMethods name methods)

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
