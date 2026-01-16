{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Struct
  ( findStruct
  , mkError
  , getStructPos
  , checkFields
  , checkMethods
  , validateFieldType
  )
where
#else
module Rune.Semantics.Struct (findStruct) where
#endif

import Text.Printf (printf)
import Control.Monad (foldM)

import Rune.AST.Nodes
import Rune.Semantics.Type (StructStack)
import Rune.Semantics.Helper (SemanticError(..), formatSemanticError)
import Rune.Semantics.Func (inferTypeFromExpr)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

---
--- public
---

findStruct :: Program -> Either String (Program, StructStack)
findStruct (Program n defs) = do
  ss <- foldM addStruct HM.empty structs

  let finalDefs = map (\d -> case d of
                              defStruct@(DefStruct name _ _ _ _) ->
                                case HM.lookup name ss of
                                  Just updated -> updated
                                  Nothing -> defStruct
                              other -> other
                       ) defs
  Right (Program n finalDefs, ss)
  where
    structs :: [TopLevelDef]
    structs = [d | d@DefStruct{} <- defs]

    addStruct :: StructStack -> TopLevelDef -> Either String StructStack
    addStruct acc def@(DefStruct name _ methods isAbstract exts)
      | HM.member name acc =
          Left $ mkError pos
            (printf "struct '%s' to be unique" name)
            "duplicate struct definition"
      | otherwise = do
          extendedFields <- checkExtensions pos def acc
          checkedFields  <- checkFields name pos acc extendedFields
          checkedMethods <- checkMethods name pos methods
          Right $ HM.insert name
            (DefStruct name checkedFields checkedMethods isAbstract exts) acc
      where
        pos :: SourcePos
        pos = getStructPos def

    addStruct acc _ = Right acc

---
--- private
---

-- TODO: TopLevelDef does not have position info for now, need to add it later in Nodes.hs. For now, we use a dummy position.

mkError :: SourcePos -> String -> String -> String
mkError (SourcePos file line col) expected got =
  formatSemanticError $ SemanticError file line col expected got ["struct definition"]

getStructPos :: TopLevelDef -> SourcePos
getStructPos (DefStruct {}) = SourcePos "<unknown>" 0 0
getStructPos _ = SourcePos "<unknown>" 0 0

checkExtensions :: SourcePos -> TopLevelDef -> StructStack -> Either String [Field]
checkExtensions pos (DefStruct name fields _ _ (Just exts)) ss = do
  checkedExts <- mapM (checkExt pos ss name) exts
  case checkedExts of
    (ext:_) -> Right $ addBaseField fields ext
    [] -> Right fields
checkExtensions _ (DefStruct _ fields _ _ Nothing) _ = Right fields
checkExtensions pos _ _ =
  Left $ mkError pos
    "only struct definitions can have extensions"
    "invalid top-level definition"

checkExt :: SourcePos -> StructStack -> String -> String -> Either String String
checkExt pos ss sName extName =
  if HM.member extName ss
    then Right extName
    else Left $ mkError pos
      (printf "struct '%s' to extend defined struct '%s'" sName extName)
      "unknown struct in extends"

addBaseField :: [Field] -> String -> [Field]
addBaseField fields ext = Field "__base" (TypeCustom ext) Public False Nothing : fields

checkMethods :: String -> SourcePos -> [TopLevelDef] -> Either String [TopLevelDef]
checkMethods _ _ = Right

checkFields :: String -> SourcePos -> StructStack -> [Field] -> Either String [Field]
checkFields sName pos structs fields = do
  let fieldNames = map fieldName fields
      duplicates = fieldNames List.\\ List.nub fieldNames
  case duplicates of
    (dup:_) -> Left $ mkError pos (printf "field '%s' in struct '%s' to be unique" dup sName) "duplicate field"
    [] -> mapM (validateFieldType sName pos structs) fields

validateFieldType :: String -> SourcePos -> StructStack -> Field -> Either String Field
validateFieldType sName pos structs field = do
  checkedType <- validateType $ fieldType field
  case fieldDefault field of
    Nothing -> Right checkedType
    Just expr -> do
      let exprType = inferTypeFromExpr expr
          fType = fieldType field
      if exprType == fType || exprType == TypeAny || fType == TypeAny
        then Right checkedType
        else Left $ mkError pos
          (printf "field '%s' default value to have type '%s'" (fieldName field) (show fType))
          (printf "default value has type '%s'" (show exprType))
  where

    validateType :: Type -> Either String Field
    validateType TypeAny = Left $ mkError pos 
      (printf "field '%s' to have a concrete type" (fieldName field)) 
      "type 'any' is not allowed"

    validateType TypeNull = Left $ mkError pos 
      (printf "field '%s' to have a concrete type" (fieldName field)) 
      "type 'null' is not allowed"

    validateType (TypeCustom customType)
      | customType == sName            = Right field
      | HM.member customType structs   = Right field
      | otherwise =
          Left $ mkError pos
            (printf "field '%s' type '%s' to be defined" (fieldName field) customType)
            "unknown struct type"

    validateType _ = Right field
