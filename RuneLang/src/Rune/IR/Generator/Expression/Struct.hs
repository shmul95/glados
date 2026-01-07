{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Struct
  ( genStructInit,
    genAccess,
    resolveStructPtr,
    lookupFieldType,
    genInitField
  )
where
#else
module Rune.IR.Generator.Expression.Struct (genStructInit, genAccess) where
#endif

import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (newTemp)
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
-- ALLOC var_name: "Type"
genStructInit :: GenExprCallback -> String -> [(String, Expression)] -> IRGen ([IRInstruction], IROperand, IRType)
genStructInit genExpr name fields = do
  let structType = IRStruct name
  resName <- newTemp "struct" structType
  let allocInstr = IRALLOC resName structType

  fieldInstrs <- concat <$> mapM (genInitField genExpr name resName structType) fields
  return (allocInstr : fieldInstrs, IRTemp resName structType, structType)

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
