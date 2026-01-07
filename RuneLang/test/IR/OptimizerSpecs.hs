module IR.OptimizerSpecs (optimizerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.IR.Optimizer
import Rune.IR.Nodes
import qualified Data.Map as M
import Control.Monad.State

--
-- public
--

optimizerTests :: TestTree
optimizerTests = testGroup "Rune.IR.Optimizer"
  [ testRunIROptimizer
  , testOptimizeTopLevel
  , testOptimizeFunction
  , testHelpers
  , testRenameInstr
  , testSimplify
  , testOptimizationLogic
  ]

--
-- private
--

testRunIROptimizer :: TestTree
testRunIROptimizer = testGroup "runIROptimizer"
  [ testCase "Identity on empty program" $
      let prog = IRProgram "test" []
      in runIROptimizer prog @?= prog
  
  , testCase "Optimizes functions in program" $
      let body = [ IRASSIGN "x" (IRConstInt 10) IRI64
                 , IRASSIGN "y" (IRTemp "x" IRI64) IRI64
                 , IRRET (Just (IRTemp "y" IRI64))
                 ]
          func = IRFunction "main" [] (Just IRI64) body False
          prog = IRProgram "test" [IRFunctionDef func]
          
          expectedBody = [ IRRET (Just (IRConstInt 10))
                         ]
          expectedProg = IRProgram "test" [IRFunctionDef (func { irFuncBody = expectedBody })]
      in runIROptimizer prog @?= expectedProg
  ]

testOptimizeTopLevel :: TestTree
testOptimizeTopLevel = testGroup "optimizeTopLevel"
  [ testCase "Optimizes IRFunctionDef" $
      let func = IRFunction "f" [] Nothing [] False
          optFunc = func { irFuncBody = [] }
          res = optimizeTopLevel M.empty (IRFunctionDef func)
      in res @?= IRFunctionDef optFunc
      
  , testCase "Ignores other top levels (float)" $
      let glob = IRGlobalDef "pi" (IRGlobalFloatVal 3.14 IRF64)
      in optimizeTopLevel M.empty glob @?= glob

  , testCase "Ignores other top levels (string)" $
      let glob = IRGlobalDef "s" (IRGlobalStringVal "val")
      in optimizeTopLevel M.empty glob @?= glob
  ]

testOptimizeFunction :: TestTree
testOptimizeFunction = testGroup "optimizeFunction"
  [ testCase "Optimizes function body" $
      let body = [IRASSIGN "a" (IRConstInt 1) IRI64, IRRET (Just (IRTemp "a" IRI64))]
          func = IRFunction "f" [] (Just IRI64) body False
          expected = [IRRET (Just (IRConstInt 1))]
      in irFuncBody (optimizeFunction M.empty func) @?= expected
  ]

testHelpers :: TestTree
testHelpers = testGroup "Helpers"
  [ testCase "isInlineable returns true for small simple functions" $
      let body = replicate 5 (IRINC (IRTemp "x" IRI64))
          func = IRFunction "f" [] Nothing body False
      in isInlineable func @?= True
      
  , testCase "isInlineable returns false for large functions" $
      let body = replicate 20 (IRINC (IRTemp "x" IRI64))
          func = IRFunction "f" [] Nothing body False
      in isInlineable func @?= False
      
  , testCase "isInlineable returns false for control flow" $
      let body = [IRLABEL (IRLabel "l")]
          func = IRFunction "f" [] Nothing body False
      in isInlineable func @?= False

  , testCase "isControlFlow identification" $ do
      isControlFlow (IRLABEL (IRLabel "l")) @?= True
      isControlFlow (IRJUMP (IRLabel "l")) @?= True
      isControlFlow (IRJUMP_TRUE (IRConstBool True) (IRLabel "l")) @?= True
      isControlFlow (IRJUMP_FALSE (IRConstBool False) (IRLabel "l")) @?= True
      isControlFlow (IRJUMP_EQ0 (IRConstInt 0) (IRLabel "l")) @?= True
      isControlFlow (IRRET Nothing) @?= False
      isControlFlow (IRASSIGN "x" (IRConstInt 1) IRI64) @?= False

  , testCase "operandType extraction" $ do
      operandType (IRTemp "x" IRI32) @?= IRI32
      operandType (IRConstInt 1) @?= IRI64
      operandType (IRConstFloat 1.0) @?= IRF64
      operandType (IRConstChar 'c') @?= IRChar
      operandType (IRConstBool True) @?= IRBool
      operandType IRConstNull @?= IRNull
      operandType (IRParam "p" IRNull) @?= IRNull
      operandType (IRGlobal "g" IRI8) @?= IRI8

  , testCase "renameOp renaming" $ do
      renameOp "pre_" (IRTemp "x" IRI32) @?= IRTemp "pre_x" IRI32
      renameOp "pre_" (IRParam "p" IRI32) @?= IRTemp "pre_p" IRI32
      renameOp "pre_" (IRConstInt 1) @?= IRConstInt 1

  , testCase "replaceRet handling" $ do
      replaceRet "tgt" [IRRET (Just (IRConstInt 1))] @?= [IRASSIGN "tgt" (IRConstInt 1) IRI64]
      replaceRet "tgt" [IRRET Nothing] @?= [IRASSIGN "tgt" IRConstNull IRNull]
      replaceRet "tgt" [IRINC (IRTemp "x" IRI32)] @?= [IRINC (IRTemp "x" IRI32)]
  ]

testRenameInstr :: TestTree
testRenameInstr = testGroup "renameInstr"
  [ testCase "Renames all instruction types" $
      let pre = "p_"
          t = IRTemp "t" IRI32
          p = IRParam "a" IRI32
          l = IRLabel "lab"

          check instr expected = renameInstr pre instr @?= expected

      in do
        check (IRALLOC "x" IRI32) (IRALLOC "p_x" IRI32)
        check (IRSTORE t t) (IRSTORE (renameOp pre t) (renameOp pre t))
        check (IRLOAD "x" t IRI32) (IRLOAD "p_x" (renameOp pre t) IRI32)
        check (IRDEREF "x" t IRI32) (IRDEREF "p_x" (renameOp pre t) IRI32)
        check (IRGET_FIELD "x" t "f1" "f2" IRI32) (IRGET_FIELD "p_x" (renameOp pre t) "f1" "f2" IRI32)
        check (IRSET_FIELD t "f1" "f2" t) (IRSET_FIELD (renameOp pre t) "f1" "f2" (renameOp pre t))
        
        check (IRADD_OP "x" t p IRI32) (IRADD_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)
        check (IRSUB_OP "x" t p IRI32) (IRSUB_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)
        check (IRMUL_OP "x" t p IRI32) (IRMUL_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)
        check (IRDIV_OP "x" t p IRI32) (IRDIV_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)
        check (IRMOD_OP "x" t p IRI32) (IRMOD_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)
        
        check (IRCMP_EQ "x" t p) (IRCMP_EQ "p_x" (renameOp pre t) (renameOp pre p))
        check (IRCMP_NEQ "x" t p) (IRCMP_NEQ "p_x" (renameOp pre t) (renameOp pre p))
        check (IRCMP_LT "x" t p) (IRCMP_LT "p_x" (renameOp pre t) (renameOp pre p))
        check (IRCMP_LTE "x" t p) (IRCMP_LTE "p_x" (renameOp pre t) (renameOp pre p))
        check (IRCMP_GT "x" t p) (IRCMP_GT "p_x" (renameOp pre t) (renameOp pre p))
        check (IRCMP_GTE "x" t p) (IRCMP_GTE "p_x" (renameOp pre t) (renameOp pre p))

        check (IRAND_OP "x" t p IRI32) (IRAND_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)
        check (IROR_OP "x" t p IRI32) (IROR_OP "p_x" (renameOp pre t) (renameOp pre p) IRI32)

        check (IRLABEL l) (IRLABEL (IRLabel "p_lab"))
        check (IRJUMP l) (IRJUMP (IRLabel "p_lab"))
        check (IRJUMP_TRUE t l) (IRJUMP_TRUE (renameOp pre t) (IRLabel "p_lab"))
        check (IRJUMP_FALSE t l) (IRJUMP_FALSE (renameOp pre t) (IRLabel "p_lab"))
        check (IRJUMP_EQ0 t l) (IRJUMP_EQ0 (renameOp pre t) (IRLabel "p_lab"))

        check (IRCALL "x" "f" [t] (Just IRI32)) (IRCALL "p_x" "f" [renameOp pre t] (Just IRI32))
        check (IRRET (Just t)) (IRRET (Just (renameOp pre t)))
        check (IRADDR "x" "y" IRI32) (IRADDR "p_x" "p_y" IRI32)
        check (IRINC t) (IRINC (renameOp pre t))
        check (IRDEC t) (IRDEC (renameOp pre t))
        check (IRASSIGN "x" t IRI32) (IRASSIGN "p_x" (renameOp pre t) IRI32)
  ]

testSimplify :: TestTree
testSimplify = testGroup "Simplification"
  [ testCase "simplifyOp propagates constants" $
      let st = OptState (M.singleton "c" (IRConstInt 42)) M.empty False
          (res, _) = runState (simplifyOp (IRTemp "c" IRI64)) st
          (res2, _) = runState (simplifyOp (IRTemp "u" IRI64)) st
      in do
        res @?= IRConstInt 42
        res2 @?= IRTemp "u" IRI64

  , testCase "simplifyInstr simplifies operands" $
      let consts = M.fromList [("c", IRConstInt 10), ("d", IRConstInt 20)]
          st = OptState consts M.empty False
          
          c = IRTemp "c" IRI64
          d = IRTemp "d" IRI64
          
          check instr expected = fst (runState (simplifyInstr instr) st) @?= expected
          
      in do
        check (IRASSIGN "x" c IRI64) (IRASSIGN "x" (IRConstInt 10) IRI64)
        check (IRCALL "x" "f" [c, d] (Just IRI64)) (IRCALL "x" "f" [IRConstInt 10, IRConstInt 20] (Just IRI64))
        check (IRSTORE c d) (IRSTORE (IRConstInt 10) (IRConstInt 20))
        check (IRLOAD "x" c IRI64) (IRLOAD "x" (IRConstInt 10) IRI64)
        check (IRDEREF "x" c IRI64) (IRDEREF "x" (IRConstInt 10) IRI64)
        check (IRGET_FIELD "x" c "f" "f2" IRI64) (IRGET_FIELD "x" (IRConstInt 10) "f" "f2" IRI64)
        check (IRSET_FIELD c "f" "f2" d) (IRSET_FIELD (IRConstInt 10) "f" "f2" (IRConstInt 20))
        
        check (IRADD_OP "x" c d IRI64) (IRADD_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        check (IRSUB_OP "x" c d IRI64) (IRSUB_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        check (IRMUL_OP "x" c d IRI64) (IRMUL_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        check (IRDIV_OP "x" c d IRI64) (IRDIV_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        check (IRMOD_OP "x" c d IRI64) (IRMOD_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        
        check (IRCMP_EQ "x" c d) (IRCMP_EQ "x" (IRConstInt 10) (IRConstInt 20))
        check (IRCMP_NEQ "x" c d) (IRCMP_NEQ "x" (IRConstInt 10) (IRConstInt 20))
        check (IRCMP_LT "x" c d) (IRCMP_LT "x" (IRConstInt 10) (IRConstInt 20))
        check (IRCMP_LTE "x" c d) (IRCMP_LTE "x" (IRConstInt 10) (IRConstInt 20))
        check (IRCMP_GT "x" c d) (IRCMP_GT "x" (IRConstInt 10) (IRConstInt 20))
        check (IRCMP_GTE "x" c d) (IRCMP_GTE "x" (IRConstInt 10) (IRConstInt 20))
        
        check (IRAND_OP "x" c d IRI64) (IRAND_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        check (IROR_OP "x" c d IRI64) (IROR_OP "x" (IRConstInt 10) (IRConstInt 20) IRI64)
        
        check (IRJUMP_TRUE c (IRLabel "l")) (IRJUMP_TRUE (IRConstInt 10) (IRLabel "l"))
        check (IRJUMP_FALSE c (IRLabel "l")) (IRJUMP_FALSE (IRConstInt 10) (IRLabel "l"))
        check (IRJUMP_EQ0 c (IRLabel "l")) (IRJUMP_EQ0 (IRConstInt 10) (IRLabel "l"))
        
        check (IRRET (Just c)) (IRRET (Just (IRConstInt 10)))
        check (IRINC c) (IRINC (IRConstInt 10))
        check (IRDEC c) (IRDEC (IRConstInt 10))
        
        check (IRALLOC "x" IRI64) (IRALLOC "x" IRI64)
  ]

testOptimizationLogic :: TestTree
testOptimizationLogic = testGroup "Logic"
  [ testCase "emitInstr adds instruction to result" $
      let st = OptState M.empty M.empty False
          (res, _) = runState (emitInstr (IRRET Nothing) []) st
      in res @?= [IRRET Nothing]

  , testCase "optimizeBlock processes list" $
      let st = OptState M.empty M.empty False
          instrs = [IRASSIGN "x" (IRConstInt 1) IRI64, IRRET (Just (IRTemp "x" IRI64))]
          (res, _) = runState (optimizeBlock instrs) st
      in res @?= [IRRET (Just (IRConstInt 1))]

  , testCase "optimizeInstr: IRASSIGN updates consts and removes if safe" $
      let st = OptState M.empty M.empty False
          instr = IRASSIGN "x" (IRConstInt 42) IRI64
          (res, newSt) = runState (optimizeInstr instr []) st
      in do
        res @?= []
        M.lookup "x" (osConsts newSt) @?= Just (IRConstInt 42)

  , testCase "optimizeInstr: IRASSIGN kept if osKeepAssignments is True" $
      let st = OptState M.empty M.empty True
          instr = IRASSIGN "x" (IRConstInt 42) IRI64
          (res, newSt) = runState (optimizeInstr instr []) st
      in do
        res @?= [instr]
        M.lookup "x" (osConsts newSt) @?= Just (IRConstInt 42)

  , testCase "optimizeInstr: IRLABEL resets constants" $
      let st = OptState (M.singleton "x" (IRConstInt 1)) M.empty False
          instr = IRLABEL (IRLabel "l")
          (res, newSt) = runState (optimizeInstr instr []) st
      in do
        res @?= [instr]
        osConsts newSt @?= M.empty

  , testCase "optimizeInstr: Inlines simple function" $
      let callee = IRFunction "min" [] (Just IRI64) [IRRET (Just (IRConstInt 5))] False
          funcs = M.singleton "min" callee
          st = OptState M.empty funcs False
          
          instr = IRCALL "res" "min" [] (Just IRI64)
          (res, st') = runState (optimizeInstr instr []) st
          
      in do
        res @?= []
        M.lookup "res" (osConsts st') @?= Just (IRConstInt 5)

  , testCase "optimizeInstr: Does not inline complex function" $
      let body = replicate 20 (IRINC (IRTemp "x" IRI64))
          callee = IRFunction "big" [] Nothing body False
          funcs = M.singleton "big" callee
          st = OptState M.empty funcs False
          
          instr = IRCALL "res" "big" [] Nothing
          (res, _) = runState (optimizeInstr instr []) st
      in res @?= [instr]

  , testCase "inlineFunction logic" $
      let callee = IRFunction "add" [("a", IRI64)] (Just IRI64) 
                   [ IRALLOC "sum" IRI64
                   , IRADD_OP "sum" (IRParam "a" IRI64) (IRConstInt 1) IRI64
                   , IRRET (Just (IRTemp "sum" IRI64))
                   ] False
          st = OptState M.empty M.empty False
          args = [IRConstInt 10]
          
          (res, _) = runState (inlineFunction "r" "add" callee args []) st
          
          prefix = "add_r_"
          expected = [ IRALLOC (prefix ++ "sum") IRI64
                     , IRADD_OP (prefix ++ "sum") (IRConstInt 10) (IRConstInt 1) IRI64
                     ]
      in res @?= expected
  ]
