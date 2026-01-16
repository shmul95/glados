{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Call
( genCall,
  selectCallStrategy,
  shouldUnroll,
  maxNonVariadicParams,
  matchesBaseName,
  extractBaseName,
  genUnrolledCall,
  genOverloadedCall,
  findBestOverload,
  accumulateResults,
  genStandardCall,
  genArgWithContext,
  prepareArg,
  prepareParamArg,
  FunctionCallInfo,
  CallStrategy (..),
)
where
#else
module Rune.IR.Generator.Expression.Call (genCall) where
#endif

import Control.Applicative ((<|>))
import Control.Monad (zipWithM, mapAndUnzipM)
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.List (find, isInfixOf)

import Rune.AST.Nodes (Expression, Type(..), Parameter(..), paramType)
import Rune.IR.IRHelpers
  ( registerCall
  , newTemp
  , astTypeToIRType
  , irTypeToASTType
  , isFloatType
  , sizeOfIRType
  )
import Rune.Semantics.Helper
  ( hasVariadicParam
  , isVariadicParam
  , unwrapRef
  , isTypeCompatible
  , isRefParam
  )
import Rune.IR.Nodes (GenState(..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- datatypes
--

type FunctionCallInfo = (String, (Type, [Parameter]))
type StructMap = Map.Map String [(String, IRType)]

data CallStrategy
  = StandardCall
  | VariadicUnroll [FunctionCallInfo] FunctionCallInfo

  deriving (Show, Eq)

--
-- public
--

-- | genCall: generate IR for a function call (with overload + variadic support)
genCall :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  fs <- gets gsFuncStack

  let allFuncs      = HM.toList fs
      matchingFuncs = filter (\(name, _) -> matchesBaseName funcName name) allFuncs
      variadicMatch = find (\(_, (_, params)) -> hasVariadicParam params) matchingFuncs

      strategy = selectCallStrategy funcName args matchingFuncs variadicMatch

  case strategy of
    StandardCall -> genStandardCall genExpr funcName args
    VariadicUnroll matchedFuncs variadicOverload ->
      genUnrolledCall genExpr funcName matchedFuncs variadicOverload args

--
-- call strategy data & selection
--

selectCallStrategy :: String -> [Expression] -> [FunctionCallInfo] -> Maybe FunctionCallInfo -> CallStrategy
selectCallStrategy name args matchingFuncs variadicOverload =
  if shouldUnroll name args matchingFuncs variadicOverload
    then case variadicOverload of
           Just v  -> VariadicUnroll matchingFuncs v
           Nothing -> StandardCall -- should never happen due to shouldUnroll check
    else StandardCall


shouldUnroll :: String -> [Expression] -> [FunctionCallInfo] -> Maybe FunctionCallInfo -> Bool
shouldUnroll _ args matchingFuncs variadicOverload =
  case variadicOverload of
    Just _ ->
      let singleArgOverloads = filter (\(_, (_, params)) -> length params == 1 && not (hasVariadicParam params)) matchingFuncs
          should = not (null singleArgOverloads) && length matchingFuncs > 1
      in should && length args > maxNonVariadicParams matchingFuncs
    Nothing -> False


maxNonVariadicParams :: [FunctionCallInfo] -> Int
maxNonVariadicParams funcs =
  maximum (0 : [ length params | (_, (_, params)) <- funcs, not (hasVariadicParam params) ])


matchesBaseName :: String -> String -> Bool
matchesBaseName base full =
  base == full ||
  extractBaseName full == base ||
  ("_" ++ base ++ "_") `isInfixOf` ("_" ++ full ++ "_")


-- extract base name between underscores if present (Struct_method style)
extractBaseName :: String -> String
extractBaseName name =
  case break (== '_') name of
    (_, "") -> name
    (_, rest) ->
      case break (== '_') (drop 1 rest) of
        ("", "") -> name
        (baseName, _) -> baseName

--
-- unrolled variadic calls (dispatch variadic args to single-arg overloads)
--
-- ouais les fonctions sont bad longues
--

genUnrolledCall
  :: (Expression -> IRGen ([IRInstruction], IROperand, IRType))
  -> String
  -> [FunctionCallInfo]
  -> FunctionCallInfo
  -> [Expression]
  -> IRGen ([IRInstruction], IROperand, IRType)
genUnrolledCall genExpr funcName matchingFuncs (_, (retType, params)) args = do
  let normalParams  = takeWhile (not . isVariadicParam . paramType) params
      numNormalArgs = length normalParams
      variadicArgs  = drop numNormalArgs args

  -- generate IR for each variadic argument
  variadicArgsData <- mapM genExpr variadicArgs

  -- for each variadic argument, call the appropriate single-arg overload (or fallback)
  let irRetType = astTypeToIRType retType
  (allInstrsList, resultOps) <- mapAndUnzipM (genOverloadedCall genExpr funcName matchingFuncs irRetType) variadicArgsData

  -- if no variadic args were provided, that's an error in this unroll path
  case resultOps of
    [] -> throwError "No variadic arguments provided"
    [single] -> pure (concat allInstrsList, single, irRetType)
    _ -> do
      (accInstrs, finalResult) <- accumulateResults irRetType resultOps
      pure (concat allInstrsList ++ accInstrs, finalResult, irRetType)


-- find best matching overload for a single variadic argument and emit a call for it
genOverloadedCall
  :: (Expression -> IRGen ([IRInstruction], IROperand, IRType))
  -> String
  -> [FunctionCallInfo]
  -> IRType
  -> ([IRInstruction], IROperand, IRType)
  -> IRGen ([IRInstruction], IROperand)
genOverloadedCall _ baseName overloads defaultRetType (argInstrs, argOp, argType) = do
  structs <- gets gsStructs

  -- consider only single-parameter non-variadic overloads as dispatch targets
  let candidates :: [(String, Type, Parameter)]
      candidates = [ (name, ret, p) | (name, (ret, [p])) <- overloads, not (hasVariadicParam [p]) ]

      argASTType = irTypeToASTType argType

      -- search policy: exact unwrap-ref match first, then compatible match
      bestMatch = findBestOverload argASTType candidates

  findBestMatch structs bestMatch

    where

    findBestMatch :: StructMap -> Maybe (String, Type, Parameter) -> IRGen ([IRInstruction], IROperand)

    -- found a matching overload
    findBestMatch sm (Just (matchedName, retType, p)) = do
      let irRetType = astTypeToIRType retType
          (finalInstrs, finalOp) = prepareParamArg sm (paramType p) argInstrs argOp argType
      
      registerCall matchedName
      retTemp <- newTemp "t" irRetType
      let callInstr = IRCALL retTemp matchedName [finalOp] (Just irRetType)
      pure (finalInstrs ++ [callInstr], IRTemp retTemp irRetType)

    -- fallback: call base name with the argument as-is
    findBestMatch sm Nothing = do
      let (argInstrs', argOp') = prepareArg sm (argInstrs, argOp, argType)
      registerCall baseName
      retTemp <- newTemp "t" defaultRetType
      let callInstr = IRCALL retTemp baseName [argOp'] (Just defaultRetType)
      pure (argInstrs' ++ [callInstr], IRTemp retTemp defaultRetType)


-- find best overload candidate given an argument type:
-- 1) exact match on unwrapRef(paramType) == argType
-- 2) compatible match using isTypeCompatible
findBestOverload :: Type -> [(String, Type, Parameter)] -> Maybe (String, Type, Parameter)
findBestOverload argType candidates =
  find (exactMatch argType) candidates <|> find (compatibleMatch argType) candidates
  where
    exactMatch a (_, _, p) = unwrapRef (paramType p) == a
    compatibleMatch a (_, _, p) = isTypeCompatible (unwrapRef (paramType p)) a


-- combine multiple results into one (e.g., add them together)
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
-- standard call path
--

genStandardCall :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genStandardCall genExpr funcName args = do
  fs <- gets gsFuncStack
  structs <- gets gsStructs
  let funcSignature = HM.lookup funcName fs

  argsData <- case funcSignature of
    Just (_, params)
      | length params == length args ->
          zipWithM (genArgWithContext genExpr) args (map paramType params)

      | hasVariadicParam params -> do
          -- handle variadic: normal args + remaining variadic args
          let normalParams = takeWhile (not . isVariadicParam . paramType) params
              numNormalArgs = length normalParams
              (normalArgs, variadicArgs) = splitAt numNormalArgs args

          normalArgsData <- zipWithM (genArgWithContext genExpr) normalArgs (map paramType normalParams)
          variadicArgsData <- mapM genExpr variadicArgs
          pure $ normalArgsData ++ variadicArgsData

    _ -> mapM genExpr args

  let (instrs, ops) = unzip $ map (prepareArg structs) argsData
      allInstrs     = concat instrs

  retType <- case funcSignature of
    Just (rt, _) -> pure $ astTypeToIRType rt
    Nothing      -> throwError $ "IR error: Function " <> funcName <> " not found in function stack"

  registerCall funcName
  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp funcName ops (Just retType)

  pure (allInstrs <> [callInstr], IRTemp retTemp retType, retType)

--
-- argument preparation and helpers
--

-- generate argument while taking into account the expected AST type
genArgWithContext :: (Expression -> IRGen ([IRInstruction], IROperand, IRType)) -> Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
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

-- prepare argument for the final call
-- small structs (<=8 bytes) are passed by value according to System V AMD64 ABI
-- Larger structs are passed by reference (pointer)
prepareArg :: StructMap -> ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg structs (i, IRTemp n t, st@(IRStruct _))
  | sizeOfIRType structs st <= 8 = (i, IRTemp n t)  -- pass small structs by value
  | otherwise = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg _ (i, IRTemp n t, IRPtr (IRStruct _)) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg _ (i, op, _) = (i, op)

-- prepare a parameter argument, handling reference parameter types (taking addresses) or passing as-is
prepareParamArg :: StructMap -> Type -> [IRInstruction] -> IROperand -> IRType -> ([IRInstruction], IROperand)
prepareParamArg structs pType argInstrs argOp argType =
  if isRefParam pType
    then case argOp of
      IRGlobal name _ ->
        let addrTemp = "addr_" <> name -- note: using deterministic temps here for readability; production could use newTemp
        in (argInstrs ++ [IRADDR addrTemp name (IRPtr argType)], IRTemp addrTemp (IRPtr argType))

      IRTemp name _ ->
        let addrTemp = "addr_" <> name
        in (argInstrs ++ [IRADDR addrTemp name (IRPtr argType)], IRTemp addrTemp (IRPtr argType))

      _ -> -- constants and others: materialize to temp then take address
        let constTemp = "c_tmp"
            addrTemp = "addr_tmp"
        in (argInstrs ++ [IRASSIGN constTemp argOp argType, IRADDR addrTemp constTemp (IRPtr argType)], IRTemp addrTemp (IRPtr argType))

    else prepareArg structs (argInstrs, argOp, argType)
