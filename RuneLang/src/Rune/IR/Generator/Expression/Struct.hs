{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Struct
  ( genStructInit,
    genAccess,
    genAccessAssign,
    resolveStructPtr,
    lookupFieldType,
    genInitField
  )
where
#else
module Rune.IR.Generator.Expression.Struct (genStructInit, genAccess, genAccessAssign) where
#endif

import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (newTemp, getDefaultValue)
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

-- | generate IR instructions for accessing a field of a struct
-- GET_FIELD var_name, "Type", "field"
genAccess :: GenExprCallback -> Expression -> String -> IRGen ([IRInstruction], IROperand, IRType)
genAccess genExpr target field = do
  (tInstrs, tOp, tType) <- genExpr target

  (structName, ptrOp, setupInstrs) <- resolveStructPtr tOp tType

  fieldType <- lookupFieldType structName field
  resTemp <- newTemp ("f_" ++ field) fieldType

  let getInstr = IRGET_FIELD resTemp ptrOp structName field fieldType
  return (tInstrs ++ setupInstrs ++ [getInstr], IRTemp resTemp fieldType, fieldType)

-- | generate IR instructions for initializing a struct
-- ALLOC var_name: "Type" & initialize fields
genStructInit :: GenExprCallback -> String -> [(String, Expression)] -> IRGen ([IRInstruction], IROperand, IRType)
genStructInit genExpr name providedFields = do
  resName <- newTemp "struct" structType
  let allocInstr = IRALLOC resName structType

  -- get all struct fields from the struct definition
  structs <- gets gsStructs
  allFields <- case Map.lookup name structs of
    Just fields -> pure fields
    Nothing -> throwError $ "Struct '" ++ name ++ "' is not defined"

  -- generate initialization for provided fields
  fieldInstrs <- concat <$> mapM (genInitField genExpr name resName structType) providedFields

  -- generate default initialization for missing fields
  let providedNames = providedFieldNames providedFields
      missing = missingFields allFields providedNames
  defaultInstrs <- concat <$> mapM (genDefaultField name resName structType) missing

  return (allocInstr : fieldInstrs ++ defaultInstrs, IRTemp resName structType, structType)

  where
    structType :: IRType
    structType = IRStruct name

    providedFieldNames :: [(String, Expression)] -> [String]
    providedFieldNames = map fst

    missingFields :: [(String, IRType)] -> [String] -> [(String, IRType)]
    missingFields allFs provided = filter (\(fName, _) -> fName `notElem` provided) allFs

-- | generate default value initialization for a struct field
genDefaultField :: String -> String -> IRType -> (String, IRType) -> IRGen [IRInstruction]
genDefaultField sName resName sType (fName, fType) = do
  ptrName <- newTemp "p_init" (IRPtr sType)
  let addrInstr    = addrInstrOf ptrName
      defaultVal   = getDefaultValue fType
      setInstr     = setInstrOf ptrName defaultVal
  pure [addrInstr, setInstr]
  where
    addrInstrOf :: String -> IRInstruction
    addrInstrOf ptr = IRADDR ptr resName (IRPtr sType)

    setInstrOf :: String -> IROperand -> IRInstruction
    setInstrOf ptr = IRSET_FIELD (IRTemp ptr (IRPtr sType)) sName fName


--
-- private
--

resolveStructPtr :: IROperand -> IRType -> IRGen (String, IROperand, [IRInstruction])
resolveStructPtr op (IRStruct s) = case op of
  IRTemp n t -> pure (s, IRTemp ("p_" ++ n) (IRPtr t), [IRADDR ("p_" ++ n) n (IRPtr t)])
  _ -> pure (s, op, [])
resolveStructPtr op (IRPtr (IRStruct s)) = pure (s, op, [])
resolveStructPtr _ tType = throwError $ "Access on non-struct type: " ++ show tType

lookupFieldType :: String -> String -> IRGen IRType
lookupFieldType sName fName = do
  structs <- gets gsStructs
  case Map.lookup sName structs of
    Just fields ->
      case lookup fName fields of
        Just typ -> return typ
        Nothing -> throwError $ "Struct '" ++ sName ++ "' has no field named '" ++ fName ++ "'"
    Nothing -> throwError $ "Struct '" ++ sName ++ "' is not defined"

genInitField :: GenExprCallback -> String -> String -> IRType -> (String, Expression) -> IRGen [IRInstruction]
genInitField genExpr sName resName sType (fName, fExpr) = do
  (valInstrs, valOp, _) <- genExpr fExpr
  ptrName <- newTemp "p_init" (IRPtr sType)
  let addrInstr = IRADDR ptrName resName (IRPtr sType)
      setInstr = IRSET_FIELD (IRTemp ptrName (IRPtr sType)) sName fName valOp
  pure $ valInstrs ++ [addrInstr, setInstr]

-- | generate IR instructions for assigning to a struct field
-- SET_FIELD ptr, "Type", "field", value
genAccessAssign :: GenExprCallback -> Expression -> String -> Expression -> IRGen [IRInstruction]
genAccessAssign genExpr target field value = do
  (tInstrs, tOp, tType) <- genExpr target
  (vInstrs, vOp, _) <- genExpr value

  (structName, ptrOp, setupInstrs) <- resolveStructPtr tOp tType

  let setInstr = IRSET_FIELD ptrOp structName field vOp
  return (tInstrs ++ setupInstrs ++ vInstrs ++ [setInstr])
