module Rune.Semantics.Struct (
    checkFields,
    findStruct
) where

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Type (StructStack)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

---
--- public
---

findStruct :: String -> StructStack -> Either String TopLevelDef
findStruct sName structs =
  case HM.lookup sName structs of
    Just structDef -> Right structDef
    Nothing        -> Left $ printf "Unknown struct '%s'" sName

checkFields :: String -> StructStack -> [Field] -> Either String [Field]
checkFields sName structs fields = do
  let fieldNames = map fieldName fields
      duplicates = fieldNames List.\\ List.nub fieldNames
  case duplicates of
    (dup:_) -> Left $ printf "Duplicate field '%s' in struct '%s'" dup sName
    [] -> mapM (validateFieldType sName structs) fields

---
--- private
---

validateFieldType :: String -> StructStack -> Field -> Either String Field
validateFieldType sName structs field = do
  case fieldType field of
    TypeAny -> Left $
      printf "Field '%s' in struct '%s' cannot have type 'any'" (fieldName field) sName
    TypeNull -> Left $
      printf "Field '%s' in struct '%s' cannot have type 'null'" (fieldName field) sName
    TypeCustom customType ->
      if HM.member customType structs
        then Right field
        else Left $
          printf "Field '%s' in struct '%s' references unknown type '%s'"
            (fieldName field) sName customType
    _ -> Right field
