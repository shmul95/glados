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
import Control.Monad (foldM, when)

import Rune.AST.Nodes
import Rune.Semantics.Type (StructStack)
import Rune.Semantics.Helper (SemanticError(..), formatSemanticError)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

---
--- public
---

findStruct :: Program -> Either String StructStack
findStruct (Program _ defs) = do
  let structs = [d | d@DefStruct {} <- defs]
  foldM addStruct HM.empty structs
  where
    addStruct acc def@(DefStruct name fields methods) = do
      let pos = getStructPos def
      when (HM.member name acc) $
        Left $ mkError pos (printf "struct '%s' to be unique" name) "duplicate struct definition"
      checkedFields <- checkFields name pos acc fields
      checkedMethods <- checkMethods name pos methods
      Right $ HM.insert name (DefStruct name checkedFields checkedMethods) acc
    addStruct acc _ = Right acc

---
--- private
---

-- TODO: TopLevelDef does not have position info for now, need to add it later in Nodes.hs. For now, we use a dummy position.

mkError :: SourcePos -> String -> String -> String
mkError (SourcePos file line col) expected got =
  formatSemanticError $ SemanticError file line col expected got ["struct definition"]

getStructPos :: TopLevelDef -> SourcePos
getStructPos (DefStruct _ _ _) = SourcePos "<unknown>" 0 0
getStructPos _ = SourcePos "<unknown>" 0 0

checkMethods :: String -> SourcePos -> [TopLevelDef] -> Either String [TopLevelDef]
checkMethods sName pos methods = do
  let defFuncNames = [n | DefFunction n _ _ _ _ <- methods]
      funcDuplicates = defFuncNames List.\\ List.nub defFuncNames
  case funcDuplicates of
    (dup:_) -> Left $ mkError pos (printf "method '%s' in struct '%s' to be unique" dup sName) "duplicate method"
    [] -> Right methods

checkFields :: String -> SourcePos -> StructStack -> [Field] -> Either String [Field]
checkFields sName pos structs fields = do
  let fieldNames = map fieldName fields
      duplicates = fieldNames List.\\ List.nub fieldNames
  case duplicates of
    (dup:_) -> Left $ mkError pos (printf "field '%s' in struct '%s' to be unique" dup sName) "duplicate field"
    [] -> mapM (validateFieldType sName pos structs) fields

validateFieldType :: String -> SourcePos -> StructStack -> Field -> Either String Field
validateFieldType sName pos structs field = do
  case fieldType field of
    TypeAny -> Left $ mkError pos 
      (printf "field '%s' to have a concrete type" (fieldName field)) 
      "type 'any' is not allowed"
    TypeNull -> Left $ mkError pos 
      (printf "field '%s' to have a concrete type" (fieldName field)) 
      "type 'null' is not allowed"
    TypeCustom customType ->
      case customType == sName || HM.member customType structs of
        True -> Right field
        False -> Left $ mkError pos 
          (printf "field '%s' type '%s' to be defined" (fieldName field) customType) 
          "unknown struct type"
    _ -> Right field
