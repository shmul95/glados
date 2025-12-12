{-# OPTIONS_GHC -cpp #-}
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
#endif
  )
where

import Rune.IR.Nodes
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe (fromMaybe)

--
-- Types
--

type ConstMap = M.Map String IROperand
type FuncMap = M.Map String IRFunction

data OptState = OptState
  { osConsts :: ConstMap,
    osFuncs :: FuncMap,
    osKeepAssignments :: Bool
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
    roots     = if M.member "main" optFuncs then S.singleton "main" else M.keysSet optFuncs
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
optimizeFunction funcs f = f { irFuncBody = newBody }
  where
    hasCF = any isControlFlow (irFuncBody f)
    initialState = OptState M.empty funcs hasCF
    (newBody, _) = runState (optimizeBlock (irFuncBody f)) initialState

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
      renamedParams = map (\(n, t) -> (prefix <> n, t)) (irFuncParams callee)
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
  in len < 15 && not hasControlFlow && not isRecursive
  where
    isSelfCall (IRCALL _ name _ _) = name == irFuncName f
    isSelfCall _ = False

isControlFlow :: IRInstruction -> Bool
isControlFlow (IRLABEL _) = True
isControlFlow (IRJUMP _) = True
isControlFlow (IRJUMP_TRUE _ _) = True
isControlFlow (IRJUMP_FALSE _ _) = True
isControlFlow (IRJUMP_EQ0 _ _) = True
isControlFlow _ = False

simplifyInstr :: IRInstruction -> OptM IRInstruction
simplifyInstr (IRASSIGN t op ty) = IRASSIGN t <$> simplifyOp op <*> pure ty
simplifyInstr (IRCALL t f args rt) = IRCALL t f <$> mapM simplifyOp args <*> pure rt
simplifyInstr (IRSTORE addr val) = IRSTORE <$> simplifyOp addr <*> simplifyOp val
simplifyInstr (IRLOAD t addr ty) = IRLOAD t <$> simplifyOp addr <*> pure ty
simplifyInstr (IRDEREF t addr ty) = IRDEREF t <$> simplifyOp addr <*> pure ty
simplifyInstr (IRGET_FIELD t s f f2 ty) = IRGET_FIELD t <$> simplifyOp s <*> pure f <*> pure f2 <*> pure ty
simplifyInstr (IRSET_FIELD s f f2 v) = IRSET_FIELD <$> simplifyOp s <*> pure f <*> pure f2 <*> simplifyOp v
simplifyInstr (IRADD_OP t o1 o2 ty) = IRADD_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRSUB_OP t o1 o2 ty) = IRSUB_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRMUL_OP t o1 o2 ty) = IRMUL_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRDIV_OP t o1 o2 ty) = IRDIV_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRMOD_OP t o1 o2 ty) = IRMOD_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRCMP_EQ t o1 o2) = IRCMP_EQ t <$> simplifyOp o1 <*> simplifyOp o2
simplifyInstr (IRCMP_NEQ t o1 o2) = IRCMP_NEQ t <$> simplifyOp o1 <*> simplifyOp o2
simplifyInstr (IRCMP_LT t o1 o2) = IRCMP_LT t <$> simplifyOp o1 <*> simplifyOp o2
simplifyInstr (IRCMP_LTE t o1 o2) = IRCMP_LTE t <$> simplifyOp o1 <*> simplifyOp o2
simplifyInstr (IRCMP_GT t o1 o2) = IRCMP_GT t <$> simplifyOp o1 <*> simplifyOp o2
simplifyInstr (IRCMP_GTE t o1 o2) = IRCMP_GTE t <$> simplifyOp o1 <*> simplifyOp o2
simplifyInstr (IRAND_OP t o1 o2 ty) = IRAND_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IROR_OP t o1 o2 ty) = IROR_OP t <$> simplifyOp o1 <*> simplifyOp o2 <*> pure ty
simplifyInstr (IRJUMP_TRUE o l) = IRJUMP_TRUE <$> simplifyOp o <*> pure l
simplifyInstr (IRJUMP_FALSE o l) = IRJUMP_FALSE <$> simplifyOp o <*> pure l
simplifyInstr (IRJUMP_EQ0 o l) = IRJUMP_EQ0 <$> simplifyOp o <*> pure l
simplifyInstr (IRRET (Just o)) = IRRET . Just <$> simplifyOp o
simplifyInstr (IRINC o) = IRINC <$> simplifyOp o
simplifyInstr (IRDEC o) = IRDEC <$> simplifyOp o
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
renameInstr pre (IRADD_OP t o1 o2 ty) = IRADD_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRSUB_OP t o1 o2 ty) = IRSUB_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRMUL_OP t o1 o2 ty) = IRMUL_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRDIV_OP t o1 o2 ty) = IRDIV_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
renameInstr pre (IRMOD_OP t o1 o2 ty) = IRMOD_OP (pre <> t) (renameOp pre o1) (renameOp pre o2) ty
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
renameInstr pre (IRCALL t f args rt) = IRCALL (pre <> t) f (map (renameOp pre) args) rt
renameInstr pre (IRRET o) = IRRET (fmap (renameOp pre) o)
renameInstr pre (IRADDR t s ty) = IRADDR (pre <> t) (pre <> s) ty
renameInstr pre (IRINC o) = IRINC (renameOp pre o)
renameInstr pre (IRDEC o) = IRDEC (renameOp pre o)
renameInstr pre (IRASSIGN t o ty) = IRASSIGN (pre <> t) (renameOp pre o) ty

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
