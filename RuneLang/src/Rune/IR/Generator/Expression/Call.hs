{-# LANGUAGE CPP #-}

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

import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.HashMap.Strict as HM
import Data.List (minimumBy)
import Data.Ord (comparing)
import Rune.AST.Nodes (Expression, Type(..))
import Rune.IR.IRHelpers (registerCall, newTemp, astTypeToIRType, isFloatType, irTypeToASTType)
import Rune.IR.Nodes (GenState(..), IRGen, IRInstruction (..), IROperand (..), IRType (..))
import Rune.Semantics.Helper (mangleName, isTypeCompatible)

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  -- INFO: lookup function signatures
  fs <- gets gsFuncStack
  let signatures = HM.lookup funcName fs

  -- INFO: generate arguments to infer their types
  argsData <- mapM genExpr args
  let (instrs, ops, irTypes) = unzip3 argsData
      allInstrs = concat instrs
      argsTypes = map irTypeToASTType irTypes

  -- INFO: find matching signature by argument types
  let matchingSignature = signatures >>= findMatching argsTypes

  -- INFO: compute mangled function name
  actualFuncName <- computeFuncName funcName matchingSignature

  -- INFO: determine return type
  retType <- computeRetType matchingSignature

  registerCall actualFuncName
  retTemp <- newTemp "t" retType

  let callInstr = IRCALL retTemp actualFuncName ops (Just retType)

  pure (allInstrs <> [callInstr], IRTemp retTemp retType, retType)

computeFuncName :: String -> Maybe (Type, [Type]) -> IRGen String
computeFuncName funcName matchingSignature =
  case matchingSignature of
    Just (retType, paramTypes) -> pure $ mangleName funcName retType paramTypes
    Nothing -> throwError $ "IR error: Function " <> funcName <> " not found"

computeRetType :: Maybe (Type, [Type]) -> IRGen IRType
computeRetType matchingSignature =
  case matchingSignature of
    Just (rt, _) -> pure $ case rt of
      TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
      t -> astTypeToIRType t
    Nothing -> throwError "IR error: Function not found"

genArgWithContext :: GenExprCallback -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genArgWithContext genExpr expr expectedType = do
  (instrs, op, inferredType) <- genExpr expr
  let targetType = case expectedType of
        TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
        t -> astTypeToIRType t
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
      | otherwise = pure (instrs, op, inferredType)

findMatching :: [Type] -> [(Type, [Type])] -> Maybe (Type, [Type])
findMatching argsTypes signatures =
  let matches = filter (matchSignature argsTypes) signatures
  in case matches of
    [] -> Nothing
    sigs -> Just $ minimumBy (comparing (specificity argsTypes)) sigs

matchSignature :: [Type] -> (Type, [Type]) -> Bool
matchSignature argsTypes (_, paramTypes)
  | length argsTypes /= length paramTypes = False
  | otherwise = and (zipWith isTypeCompatible paramTypes argsTypes)

specificity :: [Type] -> (Type, [Type]) -> Int
specificity argsTypes (_, paramTypes) = sum $ zipWith distance paramTypes argsTypes
  where
    distance expected actual = if expected == actual then 0 else 1

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, op, _) = (i, op)
