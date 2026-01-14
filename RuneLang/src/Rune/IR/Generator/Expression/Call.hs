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

import Control.Applicative ((<|>))
import Control.Monad (zipWithM, mapAndUnzipM)
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.HashMap.Strict as HM
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe)
import Rune.AST.Nodes (Expression, Type(..), Parameter(..), paramType)
import Rune.IR.IRHelpers 
  ( registerCall, 
    newTemp, 
    astTypeToIRType, 
    irTypeToASTType,
    isFloatType
  )
import Rune.Semantics.Helper
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
  
  -- Find all overloads matching this base name
  let allFuncs      = HM.toList fs
      matchingFuncs = filter (\(name, _) -> matchesBaseName funcName name) allFuncs
      variadicMatch = find (\(_, (_, params)) -> hasVariadicParam params) matchingFuncs

  -- Decide if we should unroll or use standard call
  if shouldUnroll funcName args matchingFuncs variadicMatch
    then genUnrolledCall genExpr funcName matchingFuncs (fromMaybe (error "Logic error") variadicMatch) args
    else genStandardCall genExpr funcName args

--
-- Logic for Variadic Unrolling (e.g., dispatching multiple args to single-arg overloads)
--

genUnrolledCall :: GenExprCallback -> String -> [(String, (Type, [Parameter]))] -> (String, (Type, [Parameter])) -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genUnrolledCall genExpr funcName matchingFuncs (_, (retType, params)) args = do
  -- Variadic call that needs unrolling
  let normalParams   = takeWhile (not . isVariadicParam . paramType) params
      numNormalArgs  = length normalParams
      variadicArgs   = drop numNormalArgs args
  
  -- Generate all variadic arguments
  variadicArgsData <- mapM genExpr variadicArgs
  
  let irRetType = astTypeToIRType retType
  
  -- For each variadic argument, find the right overload based on type and call it
  (allInstrs, resultOps) <- mapAndUnzipM (genOverloadedCall genExpr funcName matchingFuncs irRetType) variadicArgsData
  
  -- Accumulate results
  case resultOps of
    [] -> throwError "No variadic arguments provided"
    [single] -> pure (concat allInstrs, single, irRetType)
    _ -> do
      (accInstrs, finalResult) <- accumulateResults irRetType resultOps
      pure (concat allInstrs ++ accInstrs, finalResult, irRetType)

--
-- Logic for Standard Function Calls
--

genStandardCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genStandardCall genExpr funcName args = do
  fs <- gets gsFuncStack
  let funcSignature = HM.lookup funcName fs
  
  argsData <- case funcSignature of
    Just (_, params)
      | length params == length args ->
          zipWithM (genArgWithContext genExpr) args (map paramType params)
      | hasVariadicParam params -> do
          -- Handle variadic args: normal args + remaining args for variadic param
          let normalParams = takeWhile (not . isVariadicParam . paramType) params
              numNormalArgs = length normalParams
              (normalArgs, variadicArgs) = splitAt numNormalArgs args
          normalArgsData <- zipWithM (genArgWithContext genExpr) normalArgs (map paramType normalParams)
          variadicArgsData <- mapM genExpr variadicArgs
          pure $ normalArgsData ++ variadicArgsData
    _ -> mapM genExpr args

  let (instrs, ops) = unzip $ map prepareArg argsData
      allInstrs     = concat instrs

  retType <- case funcSignature of
    Just (rt, _) -> pure $ astTypeToIRType rt
    Nothing      -> throwError $ "IR error: Function " <> funcName <> " not found in function stack"

  registerCall funcName
  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp funcName ops (Just retType)

  pure (allInstrs <> [callInstr], IRTemp retTemp retType, retType)

--
-- Sub-helpers for Overload Resolution & Type Matching
--

-- Find the best matching overload for an argument based on its type
genOverloadedCall :: GenExprCallback -> String -> [(String, (Type, [Parameter]))] -> IRType -> ([IRInstruction], IROperand, IRType) -> IRGen ([IRInstruction], IROperand)
genOverloadedCall _ baseName overloads defaultRetType (argInstrs, argOp, argType) = do
  -- Find best matching overload for this argument type
  -- We strictly look for single-argument non-variadic functions
  let candidates = [ (name, ret, p) | (name, (ret, [p])) <- overloads, not (hasVariadicParam [p]) ]
      argASTType = irTypeToASTType argType

      -- Find exact match first (unwrapping refs), then fallback to compatible
      exactMatch      = find (\(_, _, p) -> unwrapRef (paramType p) == argASTType) candidates
      compatibleMatch = find (\(_, _, p) -> isTypeCompatible (unwrapRef (paramType p)) argASTType) candidates
      bestMatch       = exactMatch <|> compatibleMatch

  case bestMatch of
    Just (matchedName, retType, p) -> do
      let irRetType = astTypeToIRType retType
          (finalInstrs, finalOp) = prepareParamArg (paramType p) argInstrs argOp argType

      registerCall matchedName
      retTemp <- newTemp "t" irRetType
      let callInstr = IRCALL retTemp matchedName [finalOp] (Just irRetType)
      pure (finalInstrs ++ [callInstr], IRTemp retTemp irRetType)

    Nothing -> do
      -- Fallback: use the base name (might be extern like printf)
      let (argInstrs', argOp') = prepareArg (argInstrs, argOp, argType)
      registerCall baseName
      retTemp <- newTemp "t" defaultRetType
      let callInstr = IRCALL retTemp baseName [argOp'] (Just defaultRetType)
      pure (argInstrs' ++ [callInstr], IRTemp retTemp defaultRetType)

accumulateResults :: IRType -> [IROperand] -> IRGen ([IRInstruction], IROperand)
accumulateResults _ [] = throwError "No results to accumulate"
accumulateResults _ [single] = pure ([], single)
accumulateResults irRetType (first:rest) = go first rest []
  where
    go acc [] instrs = pure (instrs, acc)
    go acc (next:remaining) instrs = do
      resultTemp <- newTemp "t" irRetType
      let addInstr = IRADD_OP resultTemp acc next irRetType
      go (IRTemp resultTemp irRetType) remaining (instrs ++ [addInstr])

--
-- Decision Helpers
--

shouldUnroll :: String -> [Expression] -> [(String, (Type, [Parameter]))] -> Maybe (String, (Type, [Parameter])) -> Bool
shouldUnroll _ args matchingFuncs variadicOverload =
    case variadicOverload of
        Just (_, (_, _)) -> 
            -- Count non-variadic single-arg overloads (these are the dispatch targets)
            let singleArgOverloads = filter (\(_, (_, params)) -> length params == 1 && not (hasVariadicParam params)) matchingFuncs
                -- Only unroll if we have dispatch targets AND we have more args than any non-variadic overload can handle
                should = not (null singleArgOverloads) && length matchingFuncs > 1
            in should && length args > maxNonVariadicParams matchingFuncs
        Nothing -> False

maxNonVariadicParams :: [(String, (Type, [Parameter]))] -> Int
maxNonVariadicParams funcs = 
  maximum (0 : [length params | (_, (_, params)) <- funcs, not (hasVariadicParam params)])

matchesBaseName :: String -> String -> Bool
matchesBaseName base full = 
  base == full || 
  extractBaseName full == base ||
  ("_" ++ base ++ "_") `isInfixOf` ("_" ++ full ++ "_")

extractBaseName :: String -> String
extractBaseName name = 
  case break (== '_') name of
    (_, "") -> name
    (_, rest) -> case break (== '_') (drop 1 rest) of
                   (baseName, _) -> baseName

--
-- Argument Preparation Helpers
--

genArgWithContext :: GenExprCallback -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
genArgWithContext genExpr expr expectedType = do
  (instrs, op, inferredType) <- genExpr expr
  let targetType = astTypeToIRType expectedType
  
  if inferredType /= targetType && needsInference op inferredType targetType
    then do
      temp <- newTemp "arg" targetType
      let assign = IRASSIGN temp op targetType
      pure (instrs <> [assign], IRTemp temp targetType, targetType)
    else 
      pure (instrs, op, inferredType)
  where
    needsInference (IRConstInt _) _ _  = True
    needsInference (IRConstChar _) _ _ = True
    needsInference (IRConstBool _) _ _ = True
    needsInference (IRGlobal _ _) infT targT = isFloatType infT && isFloatType targT
    needsInference _ _ _ = False

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, op, _) = (i, op)

-- Handles Reference types by taking address
prepareParamArg :: Type -> [IRInstruction] -> IROperand -> IRType -> ([IRInstruction], IROperand)
prepareParamArg pType argInstrs argOp argType =
  if isRefParam pType
    then case argOp of
      IRGlobal name _ -> 
        let addrTemp = "addr_" <> name -- simplified for clarity, use newTemp for safety if needed
        in (argInstrs ++ [IRADDR addrTemp name (IRPtr argType)], IRTemp addrTemp (IRPtr argType))
      IRTemp name _ -> 
        let addrTemp = "addr_" <> name
        in (argInstrs ++ [IRADDR addrTemp name (IRPtr argType)], IRTemp addrTemp (IRPtr argType))
      _ -> -- Constants
        let constTemp = "c_tmp" 
            addrTemp = "addr_tmp"
        in (argInstrs ++ [IRASSIGN constTemp argOp argType, IRADDR addrTemp constTemp (IRPtr argType)], IRTemp addrTemp (IRPtr argType))
    else prepareArg (argInstrs, argOp, argType)
