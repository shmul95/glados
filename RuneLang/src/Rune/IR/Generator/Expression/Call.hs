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

import Control.Monad (zipWithM)
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.HashMap.Strict as HM
import Rune.AST.Nodes (Expression (..), Type (..))
import Rune.IR.IRHelpers (registerCall, newTemp, astTypeToIRType, isFloatType)
import Rune.IR.Nodes (GenState(..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)
type FuncSig = (Type, [Type], Maybe Type, Bool)

--
-- public
--

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  fs <- gets gsFuncStack
  let signature = HM.lookup funcName fs

  -- INFO: expand fold expressions before processing
  expandedArgs <- expandFoldArgs args

  -- INFO: generate arguments, using parameter type context
  argData <- lowerArguments signature expandedArgs

  -- INFO: prepare fixed arguments (get addresses for structs ect...)
  let (fixedData, varData) = splitBySignature signature argData
      (fixedInstrsRaw, fixedOps) = unzip $ map prepareArg fixedData
      fixedInstrs = concat fixedInstrsRaw

  -- INFO: handle variadic arguments if present
  (varInstrs, finalOps) <- emitVariadicLayout signature varData fixedOps

  -- INFO: determine return type (should always succeed)
  -- NOTE: otherwise should never happen due to semantic analysis
  retType <- inferReturnType signature
  registerCall funcName
  retTemp <- newTemp "t" retType

  let callInstr = IRCALL retTemp funcName finalOps (Just retType)
  let allInstrs = fixedInstrs ++ varInstrs ++ [callInstr]

  pure (allInstrs, IRTemp retTemp retType, retType)

  where
    genWithCtx :: Expression -> Type -> IRGen ([IRInstruction], IROperand, IRType)
    genWithCtx = genArgWithContext genExpr

    expandFoldArgs :: [Expression] -> IRGen [Expression]
    expandFoldArgs [] = pure []
    expandFoldArgs (ExprFold _ expr : rest) = do
      -- When we see ...expr, we mark it specially for later expansion
      -- For now, just pass the inner expression but remember it needs unfolding
      restExpanded <- expandFoldArgs rest
      pure (ExprFold undefined expr : restExpanded)
    expandFoldArgs (e : rest) = do
      restExpanded <- expandFoldArgs rest
      pure (e : restExpanded)

    lowerArguments :: Maybe FuncSig -> [Expression] -> IRGen [([IRInstruction], IROperand, IRType)]
    lowerArguments sig expandedArgs' = case sig of

      -- NOTE: exact match
      Just (_, params, Nothing, _) | length expandedArgs' == length params ->
        zipWithM genWithCtx expandedArgs' params

      -- NOTE: variadic match with fold expression
      Just (_, params, Just varT, isExt) | length expandedArgs' >= length params -> do
        let (f, v) = splitAt (length params) expandedArgs'
        fixedArgs <- zipWithM genWithCtx f params
        
        -- Check if we have a single ExprFold in varargs position calling external function
        case (v, isExt) of
          ([ExprFold _ inner], True) -> do
            -- This is ...args being passed to external C function
            -- Generate the inner expression which should be the varargs array
            (instrs, op, opType) <- genExpr inner
            -- Return it marked for spreading
            pure $ fixedArgs ++ [(instrs, op, opType)]
          _ -> do
            -- Normal varargs handling
            varArgs <- mapM (`genWithCtx` varT) v
            pure $ fixedArgs ++ varArgs

      _ -> mapM genExpr expandedArgs'

    splitBySignature :: Maybe FuncSig -> [a] -> ([a], [a])
    splitBySignature (Just (_, params, Just _, _)) allData = splitAt (length params) allData
    splitBySignature _ allData = (allData, [])

    emitVariadicLayout :: Maybe FuncSig -> [([IRInstruction], IROperand, IRType)] -> [IROperand] -> IRGen ([IRInstruction], [IROperand])
    emitVariadicLayout (Just (_, _, Just varT, isExt)) vData fOps
      | not (null vData) = 
          if isExt 
            then flattenCVariadics vData fOps
            else bundleRuneVariadics varT vData fOps
    emitVariadicLayout _ _ fOps = pure ([], fOps)

    flattenCVariadics :: [([IRInstruction], IROperand, IRType)] -> [IROperand] -> IRGen ([IRInstruction], [IROperand])
    flattenCVariadics vData fOps =
      let (instrs, ops, _) = unzip3 vData 
      in pure (concat instrs, fOps ++ ops)

    bundleRuneVariadics :: Type -> [([IRInstruction], IROperand, IRType)] -> [IROperand] -> IRGen ([IRInstruction], [IROperand])
    bundleRuneVariadics varT vData fOps = do
      let (instrs, ops, _) = unzip3 vData
          irElemT = astTypeToIRType varT
          arrayT  = IRPtr (IRArray irElemT 0)
      temp <- newTemp "vargs" arrayT
      let alloc = IRALLOC_ARRAY temp irElemT (ops ++ [IRConstNull])
      pure (concat instrs ++ [alloc], fOps ++ [IRTemp temp arrayT])

    inferReturnType :: Maybe FuncSig -> IRGen IRType
    inferReturnType (Just (rt, _, _, _)) = pure $ case rt of
      TypeArray elemT -> IRPtr (IRArray (astTypeToIRType elemT) 0)
      t               -> astTypeToIRType t
    inferReturnType Nothing = 
      throwError $ "IR error: Function " <> funcName <> " not found in function stack"

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
      | otherwise =
          pure (instrs, op, inferredType)

--
-- private
--

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i <> [IRADDR ("p_" <> n) n (IRPtr t)], IRTemp ("p_" <> n) (IRPtr t))
prepareArg (i, op, _) = (i, op)
