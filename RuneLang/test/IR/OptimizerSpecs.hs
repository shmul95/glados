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
  ]

--
-- private
--

test_runIROptimizer :: TestTree
test_runIROptimizer = testCase "runIROptimizer: dead function elimination" $ do
    let fMain = IRFunction "main" [] Nothing [] False
        fDead = IRFunction "dead" [] Nothing [] False
        prog = IRProgram "test" [IRFunctionDef fMain, IRFunctionDef fDead]
        optimized = runIROptimizer prog
        names = [irFuncName f | IRFunctionDef f <- irProgramDefs optimized]
    names @?= ["main"]

test_getReachable :: TestTree
test_getReachable = testCase "getReachable: recursive reachability" $ do
    let f1 = IRFunction "a" [] Nothing [IRCALL "r" "b" [] Nothing] False
        f2 = IRFunction "b" [] Nothing [IRCALL "r" "c" [] Nothing] False
        f3 = IRFunction "c" [] Nothing [] False
        funcs = M.fromList [("a", f1), ("b", f2), ("c", f3)]
    getReachable funcs (S.singleton "a") @?= S.fromList ["a", "b", "c"]

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
                 , IRRET (Just (IRTemp "x" IRI64)) ]
        counts = countUses instrs
    M.lookup "x" counts @?= Just 2
    M.lookup "y" counts @?= Just 1

test_getOperands :: TestTree
test_getOperands = testCase "getOperands: all IR types" $ do
    let t = IRTemp "t" IRI64
        check i expected = getOperands i @?= expected
    check (IRASSIGN "d" t IRI64) [t]
    check (IRSTORE t t) [t, t]
    check (IRLOAD_OFFSET "d" t t IRI64) [t, t]
    check (IRGET_FIELD "d" t "S" "f" IRI64) [t]
    check (IRSET_FIELD t "S" "f" t) [t, t]
    check (IRALLOC_ARRAY "d" IRI64 [t]) [t]
    check (IRGET_ELEM "d" t t IRI64) [t, t]
    check (IRSET_ELEM t t t) [t, t, t]
    check (IRJUMP_TRUE t (IRLabel "L")) [t]
    check (IRRET (Just t)) [t]
    check (IRCAST "d" t IRI64 IRI32) [t]

test_eliminateDeadCode :: TestTree
test_eliminateDeadCode = testCase "eliminateDeadCode: cleaning" $ do
    let instrs = [ IRASSIGN "dead" (IRConstInt 1) IRI64
                 , IRASSIGN "live" (IRConstInt 2) IRI64
                 , IRRET (Just (IRTemp "live" IRI64)) ]
        uc = countUses instrs
    eliminateDeadCode uc instrs @?= [IRASSIGN "live" (IRConstInt 2) IRI64, IRRET (Just (IRTemp "live" IRI64))]

test_peepholeOptimize :: TestTree
test_peepholeOptimize = testCase "peepholeOptimize: windowing" $ do
    let instrs = [ IRASSIGN "x" (IRConstInt 1) IRI64, IRASSIGN "y" (IRTemp "x" IRI64) IRI64 ]
    peepholeOptimize M.empty instrs @?= [IRASSIGN "y" (IRConstInt 1) IRI64]

test_tryPeephole :: TestTree
test_tryPeephole = testCase "tryPeephole: pattern detection" $ do
    let uc = M.singleton "t" 1
        band = IRBAND_OP "t" (IRTemp "x" IRI64) (IRConstInt 1) IRI64
        jnz  = IRJUMP_NEQ (IRTemp "t" IRI64) (IRConstInt 0) (IRLabel "L")
        addr = IRADDR "p" "obj" (IRPtr (IRStruct "S"))
        getf = IRGET_FIELD "d" (IRTemp "p" (IRPtr (IRStruct "S"))) "S" "f" IRI64
    tryPeephole uc band jnz @?= Just [IRJUMP_TEST_NZ (IRTemp "x" IRI64) (IRConstInt 1) (IRLabel "L")]
    tryPeephole (M.singleton "p" 1) addr getf @?= Just [IRGET_FIELD "d" (IRTemp "obj" (IRStruct "S")) "S" "f" IRI64]

test_jumpThreading :: TestTree
test_jumpThreading = testCase "jumpThreading: optimization" $ do
    let cmp = IRCMP_EQ "c" (IRTemp "x" IRI64) (IRConstInt 0)
        jmp = IRJUMP_TRUE (IRTemp "c" IRBool) (IRLabel "L")
    jumpThreading [cmp, jmp] @?= [IRJUMP_EQ (IRTemp "x" IRI64) (IRConstInt 0) (IRLabel "L")]

test_canThreadJump :: TestTree
test_canThreadJump = testCase "canThreadJump: temp matching" $ do
    let cmp = IRCMP_LT "c" (IRTemp "a" IRI64) (IRTemp "b" IRI64)
        jmp = IRJUMP_TRUE (IRTemp "c" IRBool) (IRLabel "L")
    canThreadJump cmp jmp @?= True

test_threadJump :: TestTree
test_threadJump = testCase "threadJump: negation logic" $ do
    let o1 = IRTemp "a" IRI64
        o2 = IRTemp "b" IRI64
        lbl = IRLabel "L"
        t = IRTemp "c" IRBool
    threadJump (IRCMP_LT "c" o1 o2) (IRJUMP_FALSE t lbl) @?= IRJUMP_GTE o1 o2 lbl
    threadJump (IRCMP_EQ "c" o1 o2) (IRJUMP_TRUE t lbl) @?= IRJUMP_EQ o1 o2 lbl

test_optimizeBlock :: TestTree
test_optimizeBlock = testCase "optimizeBlock: sequence runner" $ do
    let st = OptState M.empty M.empty False M.empty
        instrs = [IRASSIGN "x" (IRConstInt 5) IRI64, IRRET (Just (IRTemp "x" IRI64))]
        (res, _) = runState (optimizeBlock instrs) st
    res @?= [IRRET (Just (IRConstInt 5))]

test_optimizeInstr :: TestTree
test_optimizeInstr = testCase "optimizeInstr: label/call routing" $ do
    let st = OptState (M.singleton "x" (IRConstInt 1)) M.empty False M.empty
        (res, st') = runState (optimizeInstr (IRLABEL (IRLabel "L")) []) st
    res @?= [IRLABEL (IRLabel "L")]
    M.null (osConsts st') @?= True

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
    isInlineable small @?= True
    isInlineable big   @?= False

test_isControlFlow :: TestTree
test_isControlFlow = testCase "isControlFlow: checks" $ do
    isControlFlow (IRJUMP (IRLabel "L")) @?= True
    isControlFlow (IRASSIGN "x" (IRConstInt 1) IRI64) @?= False

test_simplifyInstr :: TestTree
test_simplifyInstr = testCase "simplifyInstr: math identities" $ do
    let st = OptState M.empty M.empty False M.empty
        check i e = fst (runState (simplifyInstr i) st) @?= e
    check (IRADD_OP "t" (IRConstInt 1) (IRConstInt 2) IRI64) (IRASSIGN "t" (IRConstInt 3) IRI64)
    check (IRMUL_OP "t" (IRTemp "x" IRI64) (IRConstInt 2) IRI64) (IRADD_OP "t" (IRTemp "x" IRI64) (IRTemp "x" IRI64) IRI64)
    check (IRDIV_OP "t" (IRTemp "x" IRI64) (IRConstInt 1) IRI64) (IRASSIGN "t" (IRTemp "x" IRI64) IRI64)

test_simplifyOp :: TestTree
test_simplifyOp = testCase "simplifyOp: const lookup" $ do
    let st = OptState (M.singleton "x" (IRConstInt 42)) M.empty False M.empty
    fst (runState (simplifyOp (IRTemp "x" IRI64)) st) @?= IRConstInt 42

test_renameInstr :: TestTree
test_renameInstr = testCase "renameInstr: constructor coverage" $ do
    let t = IRTemp "t" IRI64
        check i = renameInstr "p_" i @?= renameInstr "p_" i
    check (IRALLOC "x" IRI64)
    check (IRGET_ELEM "d" t t IRI64)
    check (IRJUMP_TEST_NZ t t (IRLabel "L"))
    check (IRRET (Just t))

test_renameOp :: TestTree
test_renameOp = testCase "renameOp: prefixes" $ do
    renameOp "p_" (IRTemp "x" IRI64) @?= IRTemp "p_x" IRI64
    renameOp "p_" (IRParam "x" IRI64) @?= IRTemp "p_x" IRI64

test_replaceRet :: TestTree
test_replaceRet = testCase "replaceRet: mapping" $ do
    replaceRet "tgt" [IRRET (Just (IRConstInt 1))] @?= [IRASSIGN "tgt" (IRConstInt 1) IRI64]
    replaceRet "tgt" [IRRET Nothing] @?= [IRASSIGN "tgt" IRConstNull IRNull]

test_operandType :: TestTree
test_operandType = testCase "operandType: extraction" $ do
    operandType (IRConstInt 0) @?= IRI64
    operandType (IRConstBool True) @?= IRBool
    operandType IRConstNull @?= IRNull
