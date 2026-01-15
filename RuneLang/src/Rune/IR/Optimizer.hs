{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Rune.IR.Optimizer
  ( runIROptimizer,
    optimizeFunction,
#if defined(TESTING_EXPORT)
    emitInstr,
    optimizeTopLevel,
    optimizeBlock,
    optimizeInstr,
    inlineFunction,
    isInlineable,
    isControlFlow,
    simplifyInstr,
    simplifyOp,
    renameInstr,
    renameOp,
    replaceRet,
    operandType,
    OptState(..),
    OptM,
    getReachable,
    singleInstrOpt,
    countUses,
    getOperands,
    eliminateDeadCode,
    peepholeOptimize,
    tryPeephole,
    jumpThreading,
    canThreadJump,
    threadJump,
#endif
  )
where

import Rune.IR.Nodes
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first)
import Data.Bits ((.&.))

--
-- Types
--

type ConstMap = M.Map String IROperand
type FuncMap = M.Map String IRFunction
type UseCount = M.Map String Int

data OptState = OptState
  { osConsts :: ConstMap,
    osFuncs :: FuncMap,
    osKeepAssignments :: Bool,
    osUseCounts :: UseCount
  }

type OptM = State OptState

--
-- public
--

runIROptimizer :: IRProgram -> IRProgram
runIROptimizer (IRProgram name tops) = IRProgram name $ filter isAlive optimized

  where
    optimized = optimizeTopLevel (funcMap tops) <$> tops
    optFuncs  = funcMap optimized
    -- Include main and all exported functions as roots
    mainRoot  = if M.member "main" optFuncs then S.singleton "main" else S.empty
    exportRoots = S.fromList [irFuncName f | IRFunctionDef f <- optimized, irFuncIsExport f]
    roots     = if S.null mainRoot && S.null exportRoots then M.keysSet optFuncs else S.union mainRoot exportRoots
    reachable = getReachable optFuncs roots
    
    isAlive (IRFunctionDef f) = S.member (irFuncName f) reachable
    isAlive _                 = True

    funcMap = foldMap $ \case
      IRFunctionDef f -> M.singleton (irFuncName f) f
      _               -> M.empty

--
-- private
--

getReachable :: FuncMap -> S.Set String -> S.Set String
getReachable funcs = fixpoint step
  where
    step seen = S.foldr (S.union . getCalls) seen seen
    getCalls f = maybe S.empty (S.fromList . foldMap target . irFuncBody) (M.lookup f funcs)
    
    target (IRCALL _ t _ _) = [t]
    target _                = []

    fixpoint f x = let x' = f x in if S.size x == S.size x' then x else fixpoint f x'

emitInstr :: IRInstruction -> [IRInstruction] -> OptM [IRInstruction]
emitInstr inst rest = (inst :) <$> optimizeBlock rest

optimizeTopLevel :: FuncMap -> IRTopLevel -> IRTopLevel
optimizeTopLevel funcs (IRFunctionDef f) = IRFunctionDef (optimizeFunction funcs f)
optimizeTopLevel _ other = other

optimizeFunction :: FuncMap -> IRFunction -> IRFunction
optimizeFunction funcs f = f { irFuncBody = finalBody }
  where
    hasCF = any isControlFlow (irFuncBody f)
    initialState = OptState M.empty funcs hasCF M.empty
    (optimizedBody, _) = runState (optimizeBlock (irFuncBody f)) initialState
    useCounts = countUses optimizedBody
    withPeephole = peepholeOptimize useCounts optimizedBody
    withIncDec = map singleInstrOpt withPeephole  -- Convert x = x + 1 to INC x
    withJumpThread = jumpThreading withIncDec
    useCountsAfterPeep = countUses withJumpThread
    finalBody = eliminateDeadCode useCountsAfterPeep withJumpThread

-- Single instruction optimizations (e.g., x = x + 1 -> INC x)
singleInstrOpt :: IRInstruction -> IRInstruction
-- x = x + 1  ->  INC x
singleInstrOpt (IRADD_OP dest (IRTemp src ty1) (IRConstInt 1) _)
  | dest == src = IRINC (IRTemp src ty1)
-- x = 1 + x  ->  INC x
singleInstrOpt (IRADD_OP dest (IRConstInt 1) (IRTemp src ty1) _)
  | dest == src = IRINC (IRTemp src ty1)
-- x = x - 1  ->  DEC x
singleInstrOpt (IRSUB_OP dest (IRTemp src ty1) (IRConstInt 1) _)
  | dest == src = IRDEC (IRTemp src ty1)
singleInstrOpt instr = instr

-- Count variable uses
countUses :: [IRInstruction] -> UseCount
countUses = foldr countInstr M.empty
  where
    countInstr inst uc = foldr incUse uc (getOperands inst)
    incUse (IRTemp t _) m = M.insertWith (+) t 1 m
    incUse _ m = m
    
getOperands :: IRInstruction -> [IROperand]
getOperands (IRASSIGN _ op _) = [op]
getOperands (IRSTORE a v) = [a, v]
getOperands (IRLOAD _ a _) = [a]
getOperands (IRDEREF _ a _) = [a]
getOperands (IRLOAD_OFFSET _ ptr offset _) = [ptr, offset]
getOperands (IRGET_FIELD _ s _ _ _) = [s]
getOperands (IRSET_FIELD s _ _ v) = [s, v]
getOperands (IRALLOC_ARRAY _ _ ops) = ops
getOperands (IRGET_ELEM _ arr idx _) = [arr, idx]
getOperands (IRSET_ELEM arr idx val) = [arr, idx, val]
getOperands (IRADD_OP _ o1 o2 _) = [o1, o2]
getOperands (IRSUB_OP _ o1 o2 _) = [o1, o2]
getOperands (IRMUL_OP _ o1 o2 _) = [o1, o2]
getOperands (IRDIV_OP _ o1 o2 _) = [o1, o2]
getOperands (IRMOD_OP _ o1 o2 _) = [o1, o2]
getOperands (IRSHR_OP _ o1 o2 _) = [o1, o2]
getOperands (IRSHL_OP _ o1 o2 _) = [o1, o2]
getOperands (IRBAND_OP _ o1 o2 _) = [o1, o2]
getOperands (IRBNOT_OP _ o _) = [o]
getOperands (IRCMP_EQ _ o1 o2) = [o1, o2]
getOperands (IRCMP_NEQ _ o1 o2) = [o1, o2]
getOperands (IRCMP_LT _ o1 o2) = [o1, o2]
getOperands (IRCMP_LTE _ o1 o2) = [o1, o2]
getOperands (IRCMP_GT _ o1 o2) = [o1, o2]
getOperands (IRCMP_GTE _ o1 o2) = [o1, o2]
getOperands (IRAND_OP _ o1 o2 _) = [o1, o2]
getOperands (IROR_OP _ o1 o2 _) = [o1, o2]
getOperands (IRJUMP_TRUE o _) = [o]
getOperands (IRJUMP_FALSE o _) = [o]
getOperands (IRJUMP_EQ0 o _) = [o]
getOperands (IRJUMP_LT o1 o2 _) = [o1, o2]
getOperands (IRJUMP_LTE o1 o2 _) = [o1, o2]
getOperands (IRJUMP_GT o1 o2 _) = [o1, o2]
getOperands (IRJUMP_GTE o1 o2 _) = [o1, o2]
getOperands (IRJUMP_EQ o1 o2 _) = [o1, o2]
getOperands (IRJUMP_NEQ o1 o2 _) = [o1, o2]
getOperands (IRCALL _ _ args _) = args
getOperands (IRRET (Just o)) = [o]
getOperands (IRINC o) = [o]
getOperands (IRDEC o) = [o]
getOperands (IRJUMP_TEST_NZ o1 o2 _) = [o1, o2]
getOperands (IRJUMP_TEST_Z o1 o2 _) = [o1, o2]
getOperands (IRCAST _ o _ _) = [o]
getOperands _ = []

-- Dead code elimination
eliminateDeadCode :: UseCount -> [IRInstruction] -> [IRInstruction]
eliminateDeadCode uc = filter (not . isDead)
  where
    isDead (IRASSIGN t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRALLOC t _) = M.findWithDefault 0 t uc == 0
    isDead (IRLOAD t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRDEREF t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRLOAD_OFFSET t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRGET_FIELD t _ _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRALLOC_ARRAY t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRGET_ELEM t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRADD_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRSUB_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRMUL_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRDIV_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRMOD_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRSHR_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRSHL_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRBAND_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRBNOT_OP t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRCMP_EQ t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRCMP_NEQ t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRCMP_LT t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRCMP_LTE t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRCMP_GT t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRCMP_GTE t _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRAND_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IROR_OP t _ _ _) = M.findWithDefault 0 t uc == 0
    isDead (IRADDR t _ _) = M.findWithDefault 0 t uc == 0
    isDead _ = False

-- Peephole optimizations
peepholeOptimize :: UseCount -> [IRInstruction] -> [IRInstruction]
peepholeOptimize _ [] = []
peepholeOptimize _ [x] = [x]
peepholeOptimize uc (i1:i2:rest) = case tryPeephole uc i1 i2 of
  Just optimized -> peepholeOptimize uc (optimized ++ rest)
  Nothing -> i1 : peepholeOptimize uc (i2:rest)

tryPeephole :: UseCount -> IRInstruction -> IRInstruction -> Maybe [IRInstruction]
tryPeephole _ (IRASSIGN t1 op1 _) (IRASSIGN t2 (IRTemp t _) ty2)
  | t1 == t = Just [IRASSIGN t2 op1 ty2]
tryPeephole _ (IRADD_OP t1 op (IRConstInt 0) _) (IRASSIGN t2 (IRTemp t _) ty2)
  | t1 == t = Just [IRASSIGN t2 op ty2]
tryPeephole _ (IRSUB_OP t1 op (IRConstInt 0) _) (IRASSIGN t2 (IRTemp t _) ty2)
  | t1 == t = Just [IRASSIGN t2 op ty2]
tryPeephole _ (IRMUL_OP t1 _ (IRConstInt 0) _) (IRASSIGN t2 (IRTemp t _) ty2)
  | t1 == t = Just [IRASSIGN t2 (IRConstInt 0) ty2]
tryPeephole _ (IRMUL_OP t1 op (IRConstInt 1) _) (IRASSIGN t2 (IRTemp t _) ty2)
  | t1 == t = Just [IRASSIGN t2 op ty2]
tryPeephole _ (IRDIV_OP t1 op (IRConstInt 1) _) (IRASSIGN t2 (IRTemp t _) ty2)
  | t1 == t = Just [IRASSIGN t2 op ty2]
-- combine OP + ASSIGN when temp is used exactly once
tryPeephole uc (IRSHR_OP t1 op shift ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRSHR_OP t2 op shift ty1]
tryPeephole uc (IRSHL_OP t1 op shift ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRSHL_OP t2 op shift ty1]
tryPeephole uc (IRBAND_OP t1 op mask ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRBAND_OP t2 op mask ty1]
tryPeephole uc (IRMUL_OP t1 o1 o2 ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRMUL_OP t2 o1 o2 ty1]
tryPeephole uc (IRADD_OP t1 o1 o2 ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRADD_OP t2 o1 o2 ty1]
tryPeephole uc (IRSUB_OP t1 o1 o2 ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRSUB_OP t2 o1 o2 ty1]
tryPeephole uc (IRDIV_OP t1 o1 o2 ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRDIV_OP t2 o1 o2 ty1]
tryPeephole uc (IRMOD_OP t1 o1 o2 ty1) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRMOD_OP t2 o1 o2 ty1]
-- combine BAND + JUMP_NEQ 0 -> JUMP_TEST_NZ (eliminates temporary)
tryPeephole uc (IRBAND_OP t1 op mask _) (IRJUMP_NEQ (IRTemp t _) (IRConstInt 0) lbl)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRJUMP_TEST_NZ op mask lbl]
tryPeephole uc (IRBAND_OP t1 op mask _) (IRJUMP_EQ (IRTemp t _) (IRConstInt 0) lbl)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRJUMP_TEST_Z op mask lbl]

-- eliminate redundant ADDR when followed by single-use field access
-- ADDR + GET_FIELD where ADDR result used only once -> inline the base operand
tryPeephole uc (IRADDR addrTemp baseName _) (IRGET_FIELD dest (IRTemp t _) sName fName fType)
  | addrTemp == t, M.findWithDefault 0 addrTemp uc == 1 = 
      Just [IRGET_FIELD dest (IRTemp baseName (IRStruct sName)) sName fName fType]

-- ADDR + SET_FIELD where ADDR result used only once -> inline the base operand
tryPeephole uc (IRADDR addrTemp baseName _) (IRSET_FIELD (IRTemp t _) sName fName val)
  | addrTemp == t, M.findWithDefault 0 addrTemp uc == 1 = 
      Just [IRSET_FIELD (IRTemp baseName (IRStruct sName)) sName fName val]

-- GET_FIELD + IRASSIGN: combine when temp used once
tryPeephole uc (IRGET_FIELD t1 base sName fName fType) (IRASSIGN t2 (IRTemp t _) _)
  | t1 == t, M.findWithDefault 0 t1 uc == 1 = Just [IRGET_FIELD t2 base sName fName fType]
tryPeephole _ _ _ = Nothing

-- jump threading: CMP + JUMP -> Direct JUMP
jumpThreading :: [IRInstruction] -> [IRInstruction]
jumpThreading [] = []
jumpThreading [x] = [x]
jumpThreading (cmp:jmp:rest)
  | canThreadJump cmp jmp = jumpThreading (threadJump cmp jmp : rest)
jumpThreading (x:xs) = x : jumpThreading xs

canThreadJump :: IRInstruction -> IRInstruction -> Bool
canThreadJump (IRCMP_LT t _ _) (IRJUMP_FALSE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_LT t _ _) (IRJUMP_TRUE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_LTE t _ _) (IRJUMP_FALSE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_LTE t _ _) (IRJUMP_TRUE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_GT t _ _) (IRJUMP_FALSE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_GT t _ _) (IRJUMP_TRUE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_GTE t _ _) (IRJUMP_FALSE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_GTE t _ _) (IRJUMP_TRUE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_EQ t _ _) (IRJUMP_FALSE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_EQ t _ _) (IRJUMP_TRUE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_NEQ t _ _) (IRJUMP_FALSE (IRTemp t' _) _) = t == t'
canThreadJump (IRCMP_NEQ t _ _) (IRJUMP_TRUE (IRTemp t' _) _) = t == t'
canThreadJump _ _ = False

threadJump :: IRInstruction -> IRInstruction -> IRInstruction
threadJump (IRCMP_LT _ o1 o2) (IRJUMP_FALSE _ lbl) = IRJUMP_GTE o1 o2 lbl
threadJump (IRCMP_LT _ o1 o2) (IRJUMP_TRUE _ lbl) = IRJUMP_LT o1 o2 lbl
threadJump (IRCMP_LTE _ o1 o2) (IRJUMP_FALSE _ lbl) = IRJUMP_GT o1 o2 lbl
threadJump (IRCMP_LTE _ o1 o2) (IRJUMP_TRUE _ lbl) = IRJUMP_LTE o1 o2 lbl
threadJump (IRCMP_GT _ o1 o2) (IRJUMP_FALSE _ lbl) = IRJUMP_LTE o1 o2 lbl
threadJump (IRCMP_GT _ o1 o2) (IRJUMP_TRUE _ lbl) = IRJUMP_GT o1 o2 lbl
threadJump (IRCMP_GTE _ o1 o2) (IRJUMP_FALSE _ lbl) = IRJUMP_LT o1 o2 lbl
threadJump (IRCMP_GTE _ o1 o2) (IRJUMP_TRUE _ lbl) = IRJUMP_GTE o1 o2 lbl
threadJump (IRCMP_EQ _ o1 o2) (IRJUMP_FALSE _ lbl) = IRJUMP_NEQ o1 o2 lbl
threadJump (IRCMP_EQ _ o1 o2) (IRJUMP_TRUE _ lbl) = IRJUMP_EQ o1 o2 lbl
threadJump (IRCMP_NEQ _ o1 o2) (IRJUMP_FALSE _ lbl) = IRJUMP_EQ o1 o2 lbl
threadJump (IRCMP_NEQ _ o1 o2) (IRJUMP_TRUE _ lbl) = IRJUMP_NEQ o1 o2 lbl
threadJump _ other = other

optimizeBlock :: [IRInstruction] -> OptM [IRInstruction]
optimizeBlock [] = return []
optimizeBlock (inst : rest) = do
  inst' <- simplifyInstr inst
  optimizeInstr inst' rest

optimizeInstr :: IRInstruction -> [IRInstruction] -> OptM [IRInstruction]

-- remember assignment for later; remove if safe
optimizeInstr inst@(IRASSIGN target op _) rest =
  modify' (\s -> s { osConsts = M.insert target op (osConsts s) })
  >> gets osKeepAssignments
  >>= \keep -> if keep then emitInstr inst rest else optimizeBlock rest

-- reset remembered values at labels
optimizeInstr inst@(IRLABEL _) rest =
  modify' (\s -> s { osConsts = M.empty })
  >> emitInstr inst rest

-- inline small/simple function calls
optimizeInstr (IRCALL target fun args retType) rest =
  gets osFuncs >>= maybe (emitInstr simpleCall rest) tryInline . M.lookup fun
  where
    simpleCall = IRCALL target fun args retType
    tryInline callee
      | isInlineable callee = inlineFunction target fun callee args rest
      | otherwise           = emitInstr simpleCall rest
-- pass other instructions as is
optimizeInstr inst rest = emitInstr inst rest

inlineFunction :: String -> String -> IRFunction -> [IROperand] -> [IRInstruction] -> OptM [IRInstruction]
inlineFunction target fun callee args rest =
  let prefix = fun <> "_" <> target <> "_"
      renamedBody = renameInstr prefix <$> irFuncBody callee
      renamedParams = map (first (prefix <>)) $ irFuncParams callee
      assigns = zipWith (\(n, t) arg -> IRASSIGN n arg t) renamedParams args
  in optimizeBlock (assigns <> replaceRet target renamedBody <> rest)

--
-- helpers
--

isInlineable :: IRFunction -> Bool
isInlineable f =
  let body = irFuncBody f
      len = length body
      hasControlFlow = any isControlFlow body
      isRecursive = any isSelfCall body
      isExported = irFuncIsExport f
  in len < 15 && not hasControlFlow && not isRecursive && not isExported
  where
    isSelfCall (IRCALL _ name _ _) = name == irFuncName f
    isSelfCall _ = False

isControlFlow :: IRInstruction -> Bool
isControlFlow (IRLABEL _) = True
isControlFlow (IRJUMP _) = True
isControlFlow (IRJUMP_TRUE _ _) = True
isControlFlow (IRJUMP_FALSE _ _) = True
isControlFlow (IRJUMP_EQ0 _ _) = True
isControlFlow (IRJUMP_LT {}) = True
isControlFlow (IRJUMP_LTE {}) = True
isControlFlow (IRJUMP_GT {}) = True
isControlFlow (IRJUMP_GTE {}) = True
isControlFlow (IRJUMP_EQ {}) = True
isControlFlow (IRJUMP_NEQ {}) = True
isControlFlow _ = False

simplifyInstr :: IRInstruction -> OptM IRInstruction
simplifyInstr (IRASSIGN t op ty) = IRASSIGN t <$> simplifyOp op <*> pure ty
simplifyInstr (IRCALL t f args rt) = IRCALL t f <$> mapM simplifyOp args <*> pure rt
simplifyInstr (IRSTORE addr val) = IRSTORE <$> simplifyOp addr <*> simplifyOp val
simplifyInstr (IRLOAD t addr ty) = IRLOAD t <$> simplifyOp addr <*> pure ty
simplifyInstr (IRDEREF t addr ty) = IRDEREF t <$> simplifyOp addr <*> pure ty
simplifyInstr (IRGET_FIELD t s f f2 ty) = IRGET_FIELD t <$> simplifyOp s <*> pure f <*> pure f2 <*> pure ty
simplifyInstr (IRSET_FIELD s f f2 v) = IRSET_FIELD <$> simplifyOp s <*> pure f <*> pure f2 <*> simplifyOp v
simplifyInstr (IRALLOC_ARRAY t ty elems) = IRALLOC_ARRAY t ty <$> mapM simplifyOp elems
simplifyInstr (IRGET_ELEM t arr idx ty) = IRGET_ELEM t <$> simplifyOp arr <*> simplifyOp idx <*> pure ty
simplifyInstr (IRSET_ELEM arr idx val) = IRSET_ELEM <$> simplifyOp arr <*> simplifyOp idx <*> simplifyOp val
simplifyInstr (IRBNOT_OP t o ty) = IRBNOT_OP t <$> simplifyOp o <*> pure ty

simplifyInstr (IRADD_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstInt (a + b)) ty
    (IRConstInt 0, op) -> IRASSIGN t op ty
    (op, IRConstInt 0) -> IRASSIGN t op ty
    _ -> IRADD_OP t o1' o2' ty

simplifyInstr (IRSUB_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstInt (a - b)) ty
    (op, IRConstInt 0) -> IRASSIGN t op ty
    _ -> IRSUB_OP t o1' o2' ty

simplifyInstr (IRMUL_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstInt (a * b)) ty
    (_, IRConstInt 0) -> IRASSIGN t (IRConstInt 0) ty
    (IRConstInt 0, _) -> IRASSIGN t (IRConstInt 0) ty
    (op, IRConstInt 1) -> IRASSIGN t op ty
    (IRConstInt 1, op) -> IRASSIGN t op ty
    (op, IRConstInt 2) -> IRADD_OP t op op ty
    (IRConstInt 2, op) -> IRADD_OP t op op ty
    _ -> IRMUL_OP t o1' o2' ty

simplifyInstr (IRDIV_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) | b /= 0 -> IRASSIGN t (IRConstInt (a `div` b)) ty
    (op, IRConstInt 1) -> IRASSIGN t op ty
    -- division by power of 2 -> shift right (for positive divisors)
    (op, IRConstInt n) | n > 0 && isPowerOf2 n -> IRSHR_OP t op (IRConstInt (log2 n)) ty
    _ -> IRDIV_OP t o1' o2' ty

simplifyInstr (IRMOD_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) | b /= 0 -> IRASSIGN t (IRConstInt (a `mod` b)) ty
    -- modulo by power of 2 -> bitwise AND with (n-1)
    (op, IRConstInt n) | n > 0 && isPowerOf2 n -> IRBAND_OP t op (IRConstInt (n - 1)) ty
    _ -> IRMOD_OP t o1' o2' ty

simplifyInstr (IRCMP_EQ t o1 o2) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstBool (a == b)) IRBool
    _ -> IRCMP_EQ t o1' o2'

simplifyInstr (IRCMP_NEQ t o1 o2) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstBool (a /= b)) IRBool
    _ -> IRCMP_NEQ t o1' o2'

simplifyInstr (IRCMP_LT t o1 o2) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstBool (a < b)) IRBool
    _ -> IRCMP_LT t o1' o2'

simplifyInstr (IRCMP_LTE t o1 o2) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstBool (a <= b)) IRBool
    _ -> IRCMP_LTE t o1' o2'

simplifyInstr (IRCMP_GT t o1 o2) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstBool (a > b)) IRBool
    _ -> IRCMP_GT t o1' o2'

simplifyInstr (IRCMP_GTE t o1 o2) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstInt a, IRConstInt b) -> IRASSIGN t (IRConstBool (a >= b)) IRBool
    _ -> IRCMP_GTE t o1' o2'

simplifyInstr (IRAND_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstBool False, _) -> IRASSIGN t (IRConstBool False) ty
    (_, IRConstBool False) -> IRASSIGN t (IRConstBool False) ty
    (IRConstBool True, op) -> IRASSIGN t op ty
    (op, IRConstBool True) -> IRASSIGN t op ty
    _ -> IRAND_OP t o1' o2' ty

simplifyInstr (IROR_OP t o1 o2 ty) = do
  o1' <- simplifyOp o1
  o2' <- simplifyOp o2
  pure $ case (o1', o2') of
    (IRConstBool True, _) -> IRASSIGN t (IRConstBool True) ty
    (_, IRConstBool True) -> IRASSIGN t (IRConstBool True) ty
    (IRConstBool False, op) -> IRASSIGN t op ty
    (op, IRConstBool False) -> IRASSIGN t op ty
    _ -> IROR_OP t o1' o2' ty
simplifyInstr (IRJUMP_TRUE o l) = IRJUMP_TRUE <$> simplifyOp o <*> pure l
simplifyInstr (IRJUMP_FALSE o l) = IRJUMP_FALSE <$> simplifyOp o <*> pure l
simplifyInstr (IRJUMP_EQ0 o l) = IRJUMP_EQ0 <$> simplifyOp o <*> pure l
simplifyInstr (IRJUMP_LT o1 o2 l) = IRJUMP_LT <$> simplifyOp o1 <*> simplifyOp o2 <*> pure l
simplifyInstr (IRJUMP_LTE o1 o2 l) = IRJUMP_LTE <$> simplifyOp o1 <*> simplifyOp o2 <*> pure l
simplifyInstr (IRJUMP_GT o1 o2 l) = IRJUMP_GT <$> simplifyOp o1 <*> simplifyOp o2 <*> pure l
simplifyInstr (IRJUMP_GTE o1 o2 l) = IRJUMP_GTE <$> simplifyOp o1 <*> simplifyOp o2 <*> pure l
simplifyInstr (IRJUMP_EQ o1 o2 l) = IRJUMP_EQ <$> simplifyOp o1 <*> simplifyOp o2 <*> pure l
simplifyInstr (IRJUMP_NEQ o1 o2 l) = IRJUMP_NEQ <$> simplifyOp o1 <*> simplifyOp o2 <*> pure l
simplifyInstr (IRRET (Just o)) = IRRET . Just <$> simplifyOp o
simplifyInstr (IRINC o) = IRINC <$> simplifyOp o
simplifyInstr (IRDEC o) = IRDEC <$> simplifyOp o
simplifyInstr (IRBAND_OP t o1 o2 ty) = IRBAND_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRSHR_OP t o1 o2 ty) = IRSHR_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRSHL_OP t o1 o2 ty) = IRSHL_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRLOAD_OFFSET t ptr offset ty) = IRLOAD_OFFSET t <$> simplifyOp ptr <*> simplifyOp offset <*> pure ty
simplifyInstr (IRCAST t o fromTy toTy) = IRCAST t <$> simplifyOp o <*> pure fromTy <*> pure toTy
simplifyInstr other = pure other

simplifyOp :: IROperand -> OptM IROperand
simplifyOp op@(IRTemp t _) = gets osConsts >>= \m -> pure $ fromMaybe op $ M.lookup t m
simplifyOp other = pure other

renameInstr :: String -> IRInstruction -> IRInstruction
renameInstr pre (IRALLOC t ty) = IRALLOC (pre <> t) ty
renameInstr pre (IRSTORE a b) = IRSTORE (renameOp pre a) (renameOp pre b)
renameInstr pre (IRLOAD t a ty) = IRLOAD (pre <> t) (renameOp pre a) ty
renameInstr pre (IRDEREF t a ty) = IRDEREF (pre <> t) (renameOp pre a) ty
renameInstr pre (IRGET_FIELD t s f1 f2 ty) = IRGET_FIELD (pre <> t) (renameOp pre s) f1 f2 ty
renameInstr pre (IRSET_FIELD s f1 f2 v) = IRSET_FIELD (renameOp pre s) f1 f2 (renameOp pre v)
renameInstr pre (IRALLOC_ARRAY t ty elems) = IRALLOC_ARRAY (pre <> t) ty (map (renameOp pre) elems)
renameInstr pre (IRGET_ELEM t arr idx ty) = IRGET_ELEM (pre <> t) (renameOp pre arr) (renameOp pre idx) ty
renameInstr pre (IRSET_ELEM arr idx val) = IRSET_ELEM (renameOp pre arr) (renameOp pre idx) (renameOp pre val)
renameInstr pre (IRADD_OP t o1 o2 ty) = IRADD_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRSUB_OP t o1 o2 ty) = IRSUB_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRMUL_OP t o1 o2 ty) = IRMUL_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRDIV_OP t o1 o2 ty) = IRDIV_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRMOD_OP t o1 o2 ty) = IRMOD_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRSHR_OP t o1 o2 ty) = IRSHR_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRSHL_OP t o1 o2 ty) = IRSHL_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRBAND_OP t o1 o2 ty) = IRBAND_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRBNOT_OP t o ty) = IRBNOT_OP (pre <> t) (renameOp pre o) ty
renameInstr pre (IRLOAD_OFFSET t ptr offset ty) = IRLOAD_OFFSET (pre <> t) (renameOp pre ptr) (renameOp pre offset) ty
renameInstr pre (IRCMP_EQ t o1 o2) = IRCMP_EQ (pre <> t) (renameOp pre o1) (renameOp pre o2)
renameInstr pre (IRCMP_NEQ t o1 o2) = IRCMP_NEQ (pre <> t) (renameOp pre o1) (renameOp pre o2)
renameInstr pre (IRCMP_LT t o1 o2) = IRCMP_LT (pre <> t) (renameOp pre o1) (renameOp pre o2)
renameInstr pre (IRCMP_LTE t o1 o2) = IRCMP_LTE (pre <> t) (renameOp pre o1) (renameOp pre o2)
renameInstr pre (IRCMP_GT t o1 o2) = IRCMP_GT (pre <> t) (renameOp pre o1) (renameOp pre o2)
renameInstr pre (IRCMP_GTE t o1 o2) = IRCMP_GTE (pre <> t) (renameOp pre o1) (renameOp pre o2)
renameInstr pre (IRAND_OP t o1 o2 ty) = IRAND_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IROR_OP t o1 o2 ty) = IROR_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRLABEL (IRLabel l)) = IRLABEL (IRLabel (pre <> l))
renameInstr pre (IRJUMP (IRLabel l)) = IRJUMP (IRLabel (pre <> l))
renameInstr pre (IRJUMP_TRUE o (IRLabel l)) = IRJUMP_TRUE (renameOp pre o) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_FALSE o (IRLabel l)) = IRJUMP_FALSE (renameOp pre o) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_EQ0 o (IRLabel l)) = IRJUMP_EQ0 (renameOp pre o) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_LT o1 o2 (IRLabel l)) = IRJUMP_LT (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_LTE o1 o2 (IRLabel l)) = IRJUMP_LTE (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_GT o1 o2 (IRLabel l)) = IRJUMP_GT (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_GTE o1 o2 (IRLabel l)) = IRJUMP_GTE (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_EQ o1 o2 (IRLabel l)) = IRJUMP_EQ (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_NEQ o1 o2 (IRLabel l)) = IRJUMP_NEQ (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_TEST_NZ o1 o2 (IRLabel l)) = IRJUMP_TEST_NZ (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRJUMP_TEST_Z o1 o2 (IRLabel l)) = IRJUMP_TEST_Z (renameOp pre o1) (renameOp pre o2) (IRLabel (pre <> l))
renameInstr pre (IRCALL t f args rt) = IRCALL (pre <> t) f (map (renameOp pre) args) rt
renameInstr pre (IRRET o) = IRRET (fmap (renameOp pre) o)
renameInstr pre (IRADDR t s ty) = IRADDR (pre <> t) (pre <> s) ty
renameInstr pre (IRINC o) = IRINC (renameOp pre o)
renameInstr pre (IRDEC o) = IRDEC (renameOp pre o)
renameInstr pre (IRASSIGN t o ty) = IRASSIGN (pre <> t) (renameOp pre o) ty
renameInstr pre (IRCAST t o fromTy toTy) = IRCAST (pre <> t) (renameOp pre o) fromTy toTy

renameOp :: String -> IROperand -> IROperand
renameOp pre (IRTemp t ty) = IRTemp (pre <> t) ty
renameOp pre (IRParam p ty) = IRTemp (pre <> p) ty
renameOp _ other = other

replaceRet :: String -> [IRInstruction] -> [IRInstruction]
replaceRet target = map go
  where
    go (IRRET (Just val)) = IRASSIGN target val (operandType val)
    go (IRRET Nothing) = IRASSIGN target IRConstNull IRNull
    go other = other

operandType :: IROperand -> IRType
operandType (IRTemp _ t) = t
operandType (IRConstInt _) = IRI64
operandType (IRConstFloat _) = IRF64
operandType (IRConstChar _) = IRChar
operandType (IRConstBool _) = IRBool
operandType IRConstNull = IRNull
operandType (IRParam _ t) = t
operandType (IRGlobal _ t) = t

-- Check if n is a power of 2
isPowerOf2 :: Int -> Bool
isPowerOf2 n = n > 0 && (n .&. (n - 1)) == 0

-- Calculate log2 for powers of 2
log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)
