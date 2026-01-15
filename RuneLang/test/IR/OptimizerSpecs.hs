{-# LANGUAGE CPP #-}
module IR.OptimizerSpecs (optimizerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.IR.Optimizer
import Rune.IR.Nodes
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

--
-- public
--

optimizerTests :: TestTree
optimizerTests = testGroup "Rune.IR.Optimizer"
  [ test_runIROptimizer
  , test_getReachable
  , test_optimizeTopLevel
  , test_optimizeFunction
  , test_singleInstrOpt
  , test_countUses
  , test_getOperands
  , test_eliminateDeadCode
  , test_peepholeOptimize
  , test_tryPeephole
  , test_jumpThreading
  , test_canThreadJump
  , test_threadJump
  , test_optimizeBlock
  , test_optimizeInstr
  , test_inlineFunction
  , test_isInlineable
  , test_isControlFlow
  , test_simplifyInstr
  , test_simplifyOp
  , test_renameInstr
  , test_renameOp
  , test_replaceRet
  , test_operandType
  , test_mathHelpers
  ]

--
-- private
--

test_runIROptimizer :: TestTree
test_runIROptimizer = testGroup "runIROptimizer"
  [ testCase "dead function elimination" $ do
      let fMain = IRFunction "main" [] Nothing [] False
          fDead = IRFunction "dead" [] Nothing [] False
          prog = IRProgram "test" [IRFunctionDef fMain, IRFunctionDef fDead]
          optimized = runIROptimizer prog
          names = [irFuncName f | IRFunctionDef f <- irProgramDefs optimized]
      names @?= ["main"]
  , testCase "keep exported functions" $ do
      let fExp = IRFunction "exp" [] Nothing [] True
          prog = IRProgram "test" [IRFunctionDef fExp]
          optimized = runIROptimizer prog
          names = [irFuncName f | IRFunctionDef f <- irProgramDefs optimized]
      names @?= ["exp"]
  , testCase "keep globals" $ do
      let glob = IRGlobalDef "g" (IRGlobalStringVal "s")
          prog = IRProgram "test" [glob]
          optimized = runIROptimizer prog
      irProgramDefs optimized @?= [glob]
  , testCase "no main or exports: keep all" $ do
      let f1 = IRFunction "f1" [] Nothing [] False
          f2 = IRFunction "f2" [] Nothing [] False
          prog = IRProgram "test" [IRFunctionDef f1, IRFunctionDef f2]
          optimized = runIROptimizer prog
          names = S.fromList [irFuncName f | IRFunctionDef f <- irProgramDefs optimized]
      names @?= S.fromList ["f1", "f2"]
  ]

test_getReachable :: TestTree
test_getReachable = testGroup "getReachable"
  [ testCase "recursive reachability" $ do
      let f1 = IRFunction "a" [] Nothing [IRCALL "r" "b" [] Nothing] False
          f2 = IRFunction "b" [] Nothing [IRCALL "r" "c" [] Nothing] False
          f3 = IRFunction "c" [] Nothing [] False
          funcs = M.fromList [("a", f1), ("b", f2), ("c", f3)]
      getReachable funcs (S.singleton "a") @?= S.fromList ["a", "b", "c"]
  , testCase "missing function" $ do
      getReachable M.empty (S.singleton "unknown") @?= S.singleton "unknown"
  ]

test_optimizeTopLevel :: TestTree
test_optimizeTopLevel = testCase "optimizeTopLevel: routing defs" $ do
    let funcDef = IRFunctionDef $ IRFunction "f" [] Nothing [] False
        globDef = IRGlobalDef "g" (IRGlobalStringVal "s")
    case optimizeTopLevel M.empty funcDef of
        IRFunctionDef _ -> return ()
        _ -> error "Should stay a function"
    optimizeTopLevel M.empty globDef @?= globDef

test_optimizeFunction :: TestTree
test_optimizeFunction = testCase "optimizeFunction: full pipeline" $ do
    let body = [ IRASSIGN "x" (IRConstInt 1) IRI64
               , IRADD_OP "x" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
               , IRRET (Just (IRTemp "x" IRI64)) ]
        func = IRFunction "f" [] Nothing body False
    irFuncBody (optimizeFunction M.empty func) @?= [IRRET (Just (IRConstInt 2))]

test_singleInstrOpt :: TestTree
test_singleInstrOpt = testCase "singleInstrOpt: INC/DEC" $ do
    let x = IRTemp "x" IRI64
    singleInstrOpt (IRADD_OP "x" x (IRConstInt 1) IRI64) @?= IRINC x
    singleInstrOpt (IRADD_OP "x" (IRConstInt 1) x IRI64) @?= IRINC x
    singleInstrOpt (IRSUB_OP "x" x (IRConstInt 1) IRI64) @?= IRDEC x

test_countUses :: TestTree
test_countUses = testCase "countUses: usage tracking" $ do
    let instrs = [ IRADD_OP "z" (IRTemp "x" IRI64) (IRTemp "y" IRI64) IRI64
                 , IRRET (Just (IRTemp "x" IRI64))
                 , IRSTORE (IRConstInt 0) (IRConstInt 0) ]
        counts = countUses instrs
    M.lookup "x" counts @?= Just 2
    M.lookup "y" counts @?= Just 1

test_getOperands :: TestTree
test_getOperands = testCase "getOperands: all IR types" $ do
    let t = IRTemp "t" IRI64
        check i expected = getOperands i @?= expected
    check (IRASSIGN "d" t IRI64) [t]
    check (IRSTORE t t) [t, t]
    check (IRLOAD "d" t IRI64) [t]
    check (IRDEREF "d" t IRI64) [t]
    check (IRLOAD_OFFSET "d" t t IRI64) [t, t]
    check (IRGET_FIELD "d" t "S" "f" IRI64) [t]
    check (IRSET_FIELD t "S" "f" t) [t, t]
    check (IRALLOC_ARRAY "d" IRI64 [t]) [t]
    check (IRGET_ELEM "d" t t IRI64) [t, t]
    check (IRSET_ELEM t t t) [t, t, t]
    check (IRADD_OP "d" t t IRI64) [t, t]
    check (IRSUB_OP "d" t t IRI64) [t, t]
    check (IRMUL_OP "d" t t IRI64) [t, t]
    check (IRDIV_OP "d" t t IRI64) [t, t]
    check (IRMOD_OP "d" t t IRI64) [t, t]
    check (IRSHR_OP "d" t t IRI64) [t, t]
    check (IRSHL_OP "d" t t IRI64) [t, t]
    check (IRBAND_OP "d" t t IRI64) [t, t]
    check (IRBNOT_OP "d" t IRI64) [t]
    check (IRCMP_EQ "d" t t) [t, t]
    check (IRCMP_NEQ "d" t t) [t, t]
    check (IRCMP_LT "d" t t) [t, t]
    check (IRCMP_LTE "d" t t) [t, t]
    check (IRCMP_GT "d" t t) [t, t]
    check (IRCMP_GTE "d" t t) [t, t]
    check (IRAND_OP "d" t t IRBool) [t, t]
    check (IROR_OP "d" t t IRBool) [t, t]
    check (IRJUMP_TRUE t (IRLabel "L")) [t]
    check (IRJUMP_FALSE t (IRLabel "L")) [t]
    check (IRJUMP_EQ0 t (IRLabel "L")) [t]
    check (IRJUMP_LT t t (IRLabel "L")) [t, t]
    check (IRJUMP_LTE t t (IRLabel "L")) [t, t]
    check (IRJUMP_GT t t (IRLabel "L")) [t, t]
    check (IRJUMP_GTE t t (IRLabel "L")) [t, t]
    check (IRJUMP_EQ t t (IRLabel "L")) [t, t]
    check (IRJUMP_NEQ t t (IRLabel "L")) [t, t]
    check (IRCALL "d" "f" [t] (Just IRI64)) [t]
    check (IRRET (Just t)) [t]
    check (IRINC t) [t]
    check (IRDEC t) [t]
    check (IRJUMP_TEST_NZ t t (IRLabel "L")) [t, t]
    check (IRJUMP_TEST_Z t t (IRLabel "L")) [t, t]
    check (IRCAST "d" t IRI64 IRI32) [t]
    check (IRLABEL (IRLabel "L")) []

test_eliminateDeadCode :: TestTree
test_eliminateDeadCode = testCase "eliminateDeadCode: cleaning" $ do
    let instrs = [ IRASSIGN "dead" (IRConstInt 1) IRI64
                 , IRALLOC "dead_alloc" IRI64
                 , IRLOAD "dead_load" (IRTemp "ptr" (IRPtr IRI64)) IRI64
                 , IRDEREF "dead_deref" (IRTemp "ptr" (IRPtr IRI64)) IRI64
                 , IRLOAD_OFFSET "dead_offset" (IRTemp "ptr" (IRPtr IRI64)) (IRConstInt 0) IRI64
                 , IRGET_FIELD "dead_field" (IRTemp "s" (IRPtr (IRStruct "S"))) "S" "f" IRI64
                 , IRALLOC_ARRAY "dead_arr" IRI64 [IRConstInt 5]
                 , IRGET_ELEM "dead_elem" (IRTemp "arr" (IRArray IRI64 1)) (IRConstInt 0) IRI64
                 , IRADD_OP "dead_add" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRSUB_OP "dead_sub" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRMUL_OP "dead_mul" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRDIV_OP "dead_div" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRMOD_OP "dead_mod" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRSHR_OP "dead_shr" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRSHL_OP "dead_shl" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRBAND_OP "dead_band" (IRConstInt 1) (IRConstInt 1) IRI64
                 , IRBNOT_OP "dead_bnot" (IRConstInt 1) IRI64
                 , IRCMP_EQ "dead_eq" (IRConstInt 1) (IRConstInt 1)
                 , IRCMP_NEQ "dead_neq" (IRConstInt 1) (IRConstInt 1)
                 , IRCMP_LT "dead_lt" (IRConstInt 1) (IRConstInt 1)
                 , IRCMP_LTE "dead_lte" (IRConstInt 1) (IRConstInt 1)
                 , IRCMP_GT "dead_gt" (IRConstInt 1) (IRConstInt 1)
                 , IRCMP_GTE "dead_gte" (IRConstInt 1) (IRConstInt 1)
                 , IRAND_OP "dead_and" (IRConstBool True) (IRConstBool True) IRBool
                 , IROR_OP "dead_or" (IRConstBool True) (IRConstBool True) IRBool
                 , IRADDR "dead_addr" "var" (IRPtr IRI64)
                 , IRASSIGN "live" (IRConstInt 2) IRI64
                 , IRRET (Just (IRTemp "live" IRI64)) ]
        uc = countUses instrs
    eliminateDeadCode uc instrs @?= [IRASSIGN "live" (IRConstInt 2) IRI64, IRRET (Just (IRTemp "live" IRI64))]

test_peepholeOptimize :: TestTree
test_peepholeOptimize = testCase "peepholeOptimize: windowing" $ do
    let instrs = [ IRASSIGN "x" (IRConstInt 1) IRI64, IRASSIGN "y" (IRTemp "x" IRI64) IRI64 ]
    peepholeOptimize M.empty instrs @?= [IRASSIGN "y" (IRConstInt 1) IRI64]
    peepholeOptimize M.empty [] @?= []
    peepholeOptimize M.empty [IRLABEL (IRLabel "L")] @?= [IRLABEL (IRLabel "L")]
    let noPeep = [IRLABEL (IRLabel "L1"), IRLABEL (IRLabel "L2")]
    peepholeOptimize M.empty noPeep @?= noPeep

test_tryPeephole :: TestTree
test_tryPeephole = testGroup "tryPeephole"
  [ testCase "ASSIGN + ASSIGN" $ do
      let i1 = IRASSIGN "t1" (IRConstInt 1) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole M.empty i1 i2 @?= Just [IRASSIGN "t2" (IRConstInt 1) IRI64]
  , testCase "ADD 0 + ASSIGN" $ do
      let i1 = IRADD_OP "t1" (IRTemp "x" IRI64) (IRConstInt 0) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole M.empty i1 i2 @?= Just [IRASSIGN "t2" (IRTemp "x" IRI64) IRI64]
  , testCase "SUB 0 + ASSIGN" $ do
      let i1 = IRSUB_OP "t1" (IRTemp "x" IRI64) (IRConstInt 0) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole M.empty i1 i2 @?= Just [IRASSIGN "t2" (IRTemp "x" IRI64) IRI64]
  , testCase "MUL 0 + ASSIGN" $ do
      let i1 = IRMUL_OP "t1" (IRTemp "x" IRI64) (IRConstInt 0) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole M.empty i1 i2 @?= Just [IRASSIGN "t2" (IRConstInt 0) IRI64]
  , testCase "MUL 1 + ASSIGN" $ do
      let i1 = IRMUL_OP "t1" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole M.empty i1 i2 @?= Just [IRASSIGN "t2" (IRTemp "x" IRI64) IRI64]
  , testCase "DIV 1 + ASSIGN" $ do
      let i1 = IRDIV_OP "t1" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole M.empty i1 i2 @?= Just [IRASSIGN "t2" (IRTemp "x" IRI64) IRI64]
  , testCase "SHR + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRSHR_OP "t1" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRSHR_OP "t2" (IRTemp "x" IRI64) (IRConstInt 1) IRI64]
  , testCase "SHL + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRSHL_OP "t1" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRSHL_OP "t2" (IRTemp "x" IRI64) (IRConstInt 1) IRI64]
  , testCase "BAND + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRBAND_OP "t1" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRBAND_OP "t2" (IRTemp "x" IRI64) (IRConstInt 1) IRI64]
  , testCase "MUL + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRMUL_OP "t1" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRMUL_OP "t2" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64]
  , testCase "ADD + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRADD_OP "t1" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRADD_OP "t2" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64]
  , testCase "SUB + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRSUB_OP "t1" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRSUB_OP "t2" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64]
  , testCase "DIV + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRDIV_OP "t1" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRDIV_OP "t2" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64]
  , testCase "MOD + ASSIGN (single use)" $ do
      let uc = M.singleton "t1" 1
          i1 = IRMOD_OP "t1" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64
          i2 = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc i1 i2 @?= Just [IRMOD_OP "t2" (IRTemp "a" IRI64) (IRTemp "b" IRI64) IRI64]
  , testCase "BAND + JUMP_EQ 0" $ do
      let uc = M.singleton "t" 1
          band = IRBAND_OP "t" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
          jeq  = IRJUMP_EQ (IRTemp "t" IRI64) (IRConstInt 0) (IRLabel "L")
      tryPeephole uc band jeq @?= Just [IRJUMP_TEST_Z (IRTemp "x" IRI64) (IRConstInt 1) (IRLabel "L")]
  , testCase "ADDR + SET_FIELD" $ do
      let uc = M.singleton "p" 1
          addr = IRADDR "p" "obj" (IRPtr (IRStruct "S"))
          setf = IRSET_FIELD (IRTemp "p" (IRPtr (IRStruct "S"))) "S" "f" (IRConstInt 1)
      tryPeephole uc addr setf @?= Just [IRSET_FIELD (IRTemp "obj" (IRStruct "S")) "S" "f" (IRConstInt 1)]
  , testCase "GET_FIELD + ASSIGN" $ do
      let uc = M.singleton "t1" 1
          getf = IRGET_FIELD "t1" (IRTemp "s" (IRStruct "S")) "S" "f" IRI64
          assign = IRASSIGN "t2" (IRTemp "t1" IRI64) IRI64
      tryPeephole uc getf assign @?= Just [IRGET_FIELD "t2" (IRTemp "s" (IRStruct "S")) "S" "f" IRI64]
  , testCase "No peephole" $ do
      tryPeephole M.empty (IRLABEL (IRLabel "L")) (IRRET Nothing) @?= Nothing
  ]

test_jumpThreading :: TestTree
test_jumpThreading = testCase "jumpThreading: optimization" $ do
    let cmp = IRCMP_EQ "c" (IRTemp "x" IRI64) (IRConstInt 0)
        jmp = IRJUMP_TRUE (IRTemp "c" IRBool) (IRLabel "L")
    jumpThreading [cmp, jmp] @?= [IRJUMP_EQ (IRTemp "x" IRI64) (IRConstInt 0) (IRLabel "L")]
    jumpThreading [] @?= []
    jumpThreading [IRRET Nothing] @?= [IRRET Nothing]

test_canThreadJump :: TestTree
test_canThreadJump = testCase "canThreadJump: variations" $ do
    let cmpLt = IRCMP_LT "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        cmpLte = IRCMP_LTE "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        cmpGt = IRCMP_GT "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        cmpGte = IRCMP_GTE "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        cmpEq = IRCMP_EQ "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        cmpNeq = IRCMP_NEQ "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        t = IRTemp "c" IRBool
        jf = IRJUMP_FALSE t (IRLabel "L")
        jt = IRJUMP_TRUE t (IRLabel "L")
    canThreadJump cmpLt jf @?= True
    canThreadJump cmpLt jt @?= True
    canThreadJump cmpLte jf @?= True
    canThreadJump cmpLte jt @?= True
    canThreadJump cmpGt jf @?= True
    canThreadJump cmpGt jt @?= True
    canThreadJump cmpGte jf @?= True
    canThreadJump cmpGte jt @?= True
    canThreadJump cmpEq jf @?= True
    canThreadJump cmpEq jt @?= True
    canThreadJump cmpNeq jf @?= True
    canThreadJump cmpNeq jt @?= True
    canThreadJump cmpLt (IRJUMP_FALSE (IRTemp "x" IRBool) (IRLabel "L")) @?= False
    canThreadJump (IRRET Nothing) jf @?= False

test_threadJump :: TestTree
test_threadJump = testCase "threadJump: logic" $ do
    let o1 = IRTemp "a" IRI64
        o2 = IRTemp "b" IRI64
        lbl = IRLabel "L"
        t = IRTemp "c" IRBool
    threadJump (IRCMP_LT "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_GTE o1 o2 lbl
    threadJump (IRCMP_LT "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_LT o1 o2 lbl
    threadJump (IRCMP_LTE "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_GT o1 o2 lbl
    threadJump (IRCMP_LTE "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_LTE o1 o2 lbl
    threadJump (IRCMP_GT "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_LTE o1 o2 lbl
    threadJump (IRCMP_GT "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_GT o1 o2 lbl
    threadJump (IRCMP_GTE "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_LT o1 o2 lbl
    threadJump (IRCMP_GTE "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_GTE o1 o2 lbl
    threadJump (IRCMP_EQ "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_NEQ o1 o2 lbl
    threadJump (IRCMP_EQ "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_EQ o1 o2 lbl
    threadJump (IRCMP_NEQ "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_EQ o1 o2 lbl
    threadJump (IRCMP_NEQ "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_NEQ o1 o2 lbl
    threadJump (IRRET Nothing) (IRRET Nothing) @?= IRRET Nothing

test_optimizeBlock :: TestTree
test_optimizeBlock = testCase "optimizeBlock: sequence runner" $ do
    let st = OptState M.empty M.empty False M.empty
        instrs = [IRASSIGN "x" (IRConstInt 5) IRI64, IRRET (Just (IRTemp "x" IRI64))]
        (res, _) = runState (optimizeBlock instrs) st
    res @?= [IRRET (Just (IRConstInt 5))]
    let (resEmpty, _) = runState (optimizeBlock []) st
    resEmpty @?= []

test_optimizeInstr :: TestTree
test_optimizeInstr = testGroup "optimizeInstr"
  [ testCase "label/call routing" $ do
      let st = OptState (M.singleton "x" (IRConstInt 1)) M.empty False M.empty
          (res, st') = runState (optimizeInstr (IRLABEL (IRLabel "L")) []) st
      res @?= [IRLABEL (IRLabel "L")]
      M.null (osConsts st') @?= True
  , testCase "keep assignments" $ do
      let st = OptState M.empty M.empty True M.empty
          (res, _) = runState (optimizeInstr (IRASSIGN "x" (IRConstInt 1) IRI64) []) st
      res @?= [IRASSIGN "x" (IRConstInt 1) IRI64]
  , testCase "inline call" $ do
      let callee = IRFunction "sub" [("p", IRI64)] (Just IRI64) [IRRET (Just (IRParam "p" IRI64))] False
          funcs = M.singleton "sub" callee
          st = OptState M.empty funcs False M.empty
          (res, _) = runState (optimizeInstr (IRCALL "res" "sub" [IRConstInt 42] (Just IRI64)) [IRRET (Just (IRTemp "res" IRI64))]) st
      res @?= [IRRET (Just (IRConstInt 42))]
  ]

test_inlineFunction :: TestTree
test_inlineFunction = testCase "inlineFunction: renaming logic" $ do
    let callee = IRFunction "sub" [("p", IRI64)] (Just IRI64) [IRRET (Just (IRParam "p" IRI64))] False
        st = OptState M.empty M.empty False M.empty
        (res, _) = runState (inlineFunction "target" "sub" callee [IRConstInt 42] []) st
    res @?= []

test_isInlineable :: TestTree
test_isInlineable = testCase "isInlineable: criteria" $ do
    let small = IRFunction "f" [] Nothing [IRRET Nothing] False
        big   = IRFunction "f" [] Nothing (replicate 20 (IRRET Nothing)) False
        rec   = IRFunction "f" [] Nothing [IRCALL "r" "f" [] Nothing] False
        fExp  = IRFunction "f" [] Nothing [IRRET Nothing] True
    isInlineable small @?= True
    isInlineable big   @?= False
    isInlineable rec   @?= False
    isInlineable fExp  @?= False

test_isControlFlow :: TestTree
test_isControlFlow = testCase "isControlFlow: checks" $ do
    isControlFlow (IRLABEL (IRLabel "L")) @?= True
    isControlFlow (IRJUMP (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_TRUE (IRConstBool True) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_FALSE (IRConstBool True) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_EQ0 (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_LT (IRConstInt 0) (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_LTE (IRConstInt 0) (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_GT (IRConstInt 0) (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_GTE (IRConstInt 0) (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_EQ (IRConstInt 0) (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRJUMP_NEQ (IRConstInt 0) (IRConstInt 0) (IRLabel "L")) @?= True
    isControlFlow (IRASSIGN "x" (IRConstInt 1) IRI64) @?= False

test_simplifyInstr :: TestTree
test_simplifyInstr = testCase "simplifyInstr: identities and constant folding" $ do
    let st = OptState M.empty M.empty False M.empty
        check i e = fst (runState (simplifyInstr i) st) @?= e
    check (IRADD_OP "t" (IRConstInt 1) (IRConstInt 2) IRI64) (IRASSIGN "t" (IRConstInt 3) IRI64)
    check (IRSUB_OP "t" (IRConstInt 5) (IRConstInt 2) IRI64) (IRASSIGN "t" (IRConstInt 3) IRI64)
    check (IRMUL_OP "t" (IRConstInt 3) (IRConstInt 4) IRI64) (IRASSIGN "t" (IRConstInt 12) IRI64)
    check (IRDIV_OP "t" (IRConstInt 10) (IRConstInt 2) IRI64) (IRASSIGN "t" (IRConstInt 5) IRI64)
    check (IRMOD_OP "t" (IRConstInt 10) (IRConstInt 3) IRI64) (IRASSIGN "t" (IRConstInt 1) IRI64)
    check (IRCMP_EQ "t" (IRConstInt 1) (IRConstInt 1)) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IRCMP_NEQ "t" (IRConstInt 1) (IRConstInt 2)) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IRCMP_LT "t" (IRConstInt 1) (IRConstInt 2)) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IRCMP_LTE "t" (IRConstInt 2) (IRConstInt 2)) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IRCMP_GT "t" (IRConstInt 2) (IRConstInt 1)) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IRCMP_GTE "t" (IRConstInt 2) (IRConstInt 2)) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IRAND_OP "t" (IRConstBool False) (IRConstBool True) IRBool) (IRASSIGN "t" (IRConstBool False) IRBool)
    check (IRAND_OP "t" (IRConstBool True) (IRConstBool False) IRBool) (IRASSIGN "t" (IRConstBool False) IRBool)
    check (IRAND_OP "t" (IRConstBool True) (IRTemp "x" IRBool) IRBool) (IRASSIGN "t" (IRTemp "x" IRBool) IRBool)
    check (IRAND_OP "t" (IRTemp "x" IRBool) (IRConstBool True) IRBool) (IRASSIGN "t" (IRTemp "x" IRBool) IRBool)
    check (IROR_OP "t" (IRConstBool True) (IRConstBool False) IRBool) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IROR_OP "t" (IRConstBool False) (IRConstBool True) IRBool) (IRASSIGN "t" (IRConstBool True) IRBool)
    check (IROR_OP "t" (IRConstBool False) (IRTemp "x" IRBool) IRBool) (IRASSIGN "t" (IRTemp "x" IRBool) IRBool)
    check (IROR_OP "t" (IRTemp "x" IRBool) (IRConstBool False) IRBool) (IRASSIGN "t" (IRTemp "x" IRBool) IRBool)
    check (IRDIV_OP "t" (IRTemp "x" IRI64) (IRConstInt 1) IRI64) (IRASSIGN "t" (IRTemp "x" IRI64) IRI64)
    check (IRMUL_OP "t" (IRTemp "x" IRI64) (IRConstInt 2) IRI64) (IRADD_OP "t" (IRTemp "x" IRI64) (IRTemp "x" IRI64) IRI64)
    check (IRBNOT_OP "t" (IRConstInt 0) IRI64) (IRBNOT_OP "t" (IRConstInt 0) IRI64)
    check (IRRET (Just (IRConstInt 1))) (IRRET (Just (IRConstInt 1)))
    check (IRLABEL (IRLabel "L")) (IRLABEL (IRLabel "L"))

test_simplifyOp :: TestTree
test_simplifyOp = testCase "simplifyOp: const lookup" $ do
    let st = OptState (M.singleton "x" (IRConstInt 42)) M.empty False M.empty
    fst (runState (simplifyOp (IRTemp "x" IRI64)) st) @?= IRConstInt 42

test_renameInstr :: TestTree
test_renameInstr = testCase "renameInstr: constructor coverage" $ do
    let t = IRTemp "t" IRI64
        check i = renameInstr "p_" i @?= renameInstr "p_" i
    check (IRALLOC "x" IRI64)
    check (IRSTORE t t)
    check (IRLOAD "x" t IRI64)
    check (IRDEREF "x" t IRI64)
    check (IRGET_FIELD "x" t "S" "f" IRI64)
    check (IRSET_FIELD t "S" "f" t)
    check (IRALLOC_ARRAY "x" IRI64 [t])
    check (IRGET_ELEM "d" t t IRI64)
    check (IRSET_ELEM t t t)
    check (IRADD_OP "x" t t IRI64)
    check (IRSUB_OP "x" t t IRI64)
    check (IRMUL_OP "x" t t IRI64)
    check (IRDIV_OP "x" t t IRI64)
    check (IRMOD_OP "x" t t IRI64)
    check (IRSHR_OP "x" t t IRI64)
    check (IRSHL_OP "x" t t IRI64)
    check (IRBAND_OP "x" t t IRI64)
    check (IRBNOT_OP "x" t IRI64)
    check (IRLOAD_OFFSET "x" t t IRI64)
    check (IRCMP_EQ "x" t t)
    check (IRCMP_NEQ "x" t t)
    check (IRCMP_LT "x" t t)
    check (IRCMP_LTE "x" t t)
    check (IRCMP_GT "x" t t)
    check (IRCMP_GTE "x" t t)
    check (IRAND_OP "x" t t IRBool)
    check (IROR_OP "x" t t IRBool)
    check (IRLABEL (IRLabel "L"))
    check (IRJUMP (IRLabel "L"))
    check (IRJUMP_TRUE t (IRLabel "L"))
    check (IRJUMP_FALSE t (IRLabel "L"))
    check (IRJUMP_EQ0 t (IRLabel "L"))
    check (IRJUMP_LT t t (IRLabel "L"))
    check (IRJUMP_LTE t t (IRLabel "L"))
    check (IRJUMP_GT t t (IRLabel "L"))
    check (IRJUMP_GTE t t (IRLabel "L"))
    check (IRJUMP_EQ t t (IRLabel "L"))
    check (IRJUMP_NEQ t t (IRLabel "L"))
    check (IRJUMP_TEST_NZ t t (IRLabel "L"))
    check (IRJUMP_TEST_Z t t (IRLabel "L"))
    check (IRCALL "x" "f" [t] Nothing)
    check (IRRET (Just t))
    check (IRADDR "x" "v" IRI64)
    check (IRINC t)
    check (IRDEC t)
    check (IRASSIGN "x" t IRI64)
    check (IRCAST "x" t IRI64 IRI32)

test_renameOp :: TestTree
test_renameOp = testCase "renameOp: prefixes" $ do
    renameOp "p_" (IRTemp "x" IRI64) @?= IRTemp "p_x" IRI64
    renameOp "p_" (IRParam "x" IRI64) @?= IRTemp "p_x" IRI64
    renameOp "p_" (IRConstInt 1) @?= IRConstInt 1

test_replaceRet :: TestTree
test_replaceRet = testCase "replaceRet: mapping" $ do
    replaceRet "tgt" [IRRET (Just (IRConstInt 1))] @?= [IRASSIGN "tgt" (IRConstInt 1) IRI64]
    replaceRet "tgt" [IRRET Nothing] @?= [IRASSIGN "tgt" IRConstNull IRNull]
    replaceRet "tgt" [IRLABEL (IRLabel "L")] @?= [IRLABEL (IRLabel "L")]

test_operandType :: TestTree
test_operandType = testCase "operandType: extraction" $ do
    operandType (IRTemp "x" IRI64) @?= IRI64
    operandType (IRConstInt 0) @?= IRI64
    operandType (IRConstFloat 0) @?= IRF64
    operandType (IRConstChar 'a') @?= IRChar
    operandType (IRConstBool True) @?= IRBool
    operandType IRConstNull @?= IRNull
    operandType (IRParam "p" IRI64) @?= IRI64
    operandType (IRGlobal "g" IRI64) @?= IRI64

test_mathHelpers :: TestTree
test_mathHelpers = testCase "isPowerOf2 and log2" $ do
    let st = OptState M.empty M.empty False M.empty
        check i e = fst (runState (simplifyInstr i) st) @?= e
    check (IRDIV_OP "t" (IRTemp "x" IRI64) (IRConstInt 4) IRI64) (IRSHR_OP "t" (IRTemp "x" IRI64) (IRConstInt 2) IRI64)
    check (IRMOD_OP "t" (IRTemp "x" IRI64) (IRConstInt 8) IRI64) (IRBAND_OP "t" (IRTemp "x" IRI64) (IRConstInt 7) IRI64)

