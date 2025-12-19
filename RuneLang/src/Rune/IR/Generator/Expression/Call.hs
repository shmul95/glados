{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Call
  ( genCall,
    prepareArg,
    genArgWithContext
  )
where
#else
module Rune.IR.Generator.Expression.Call (genCall) where
#endif

import Control.Monad (zipWithM)
import Control.Monad.State (gets)
import qualified Data.HashMap.Strict as HM
import Rune.AST.Nodes (Expression, Type)
import Rune.IR.IRHelpers (registerCall, newTemp, astTypeToIRType, isFloatType)
import Rune.IR.Nodes (GenState(..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  fs <- gets gsFuncStack

  -- INFO: lookup for the resolved function signature
  let funcSignature = do
        sigs <- HM.lookup funcName fs
        case sigs of
          [(retType, paramTypes)] -> Just (retType, paramTypes)
          _                       -> Nothing

  -- INFO: generate arguments, using parameter type context
  argsData <- case funcSignature of
    Just (_, paramTypes)
      | length paramTypes == length args ->
          zipWithM (genArgWithContext genExpr) args paramTypes
    _ -> mapM genExpr args

  -- INFO: prepare arguments (get addresses for structs ect...)
  let (instrs, ops) = unzip $ map prepareArg argsData
      allInstrs     = concat instrs

  -- INFO: determine return type (should always succeed)
  -- NOTE: otherwise should never happen due to semantic analysis
  retType <- case funcSignature of
    Just (rt, _) -> pure $ astTypeToIRType rt
    Nothing -> error $ "IR error: Function " <> funcName <> " not found in function stack"

  registerCall funcName
  retTemp <- newTemp "t" retType

  let callInstr = IRCALL retTemp funcName ops (Just retType)

  pure (allInstrs <> [callInstr], IRTemp retTemp retType, retType)


genArgWithContext :: GenExprCallback -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genArgWithContext genExpr expr expectedType = do
  (instrs, op, inferredType) <- genExpr expr
  let targetType = astTypeToIRType expectedType
  
  inferIfNeeded instrs op inferredType targetType
  where
    needsInference (IRConstInt _) _ _ = True
    needsInference (IRConstChar _) _ _ = True
    needsInference (IRConstBool _) _ _ = True
    needsInference (IRGlobal _ _) infT targT = isFloatType infT && isFloatType targT
    needsInference _ _ _ = False

    inferIfNeeded instrs op inferredType targetType
      | inferredType /= targetType && needsInference op inferredType targetType = do
          temp <- newTemp "arg" targetType
          let assign = IRASSIGN temp op targetType
          pure (instrs <> [assign], IRTemp temp targetType, targetType)
      | otherwise =
          pure (instrs, op, inferredType)

--
-- private
--

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, op, _) = (i, op)