{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.OptimizerSpecs (optimizerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.IR.Optimizer (runIROptimizer)
import Rune.IR.Nodes (IRProgram(..), IRTopLevel(..), IRFunction(..), IRType(..), IRInstruction(..), IROperand(..))

--
-- public
--

optimizerTests :: TestTree
optimizerTests = testGroup "Rune.IR.Optimizer"
  [ testRunIROptimizer
  ]

--
-- private
--

testRunIROptimizer :: TestTree
testRunIROptimizer = testGroup "runIROptimizer"
  [ testCase "Returns program unchanged (identity)" $
      let prog = IRProgram "test" []
      in runIROptimizer prog @?= prog
  
  , testCase "Returns program with function unchanged" $
      let func = IRFunction "main" [] (Just IRNull) [IRRET Nothing]
          prog = IRProgram "test" [IRFunctionDef func]
      in runIROptimizer prog @?= prog
  
  , testCase "Returns program with global string unchanged" $
      let prog = IRProgram "test" [IRGlobalString "str" "hello"]
      in runIROptimizer prog @?= prog
  
  , testCase "Returns program with struct unchanged" $
      let prog = IRProgram "test" [IRStructDef "S" [("x", IRI32)]]
      in runIROptimizer prog @?= prog
  
  , testCase "Returns program with extern unchanged" $
      let prog = IRProgram "test" [IRExtern "printf"]
      in runIROptimizer prog @?= prog
  
  , testCase "Returns complex program unchanged" $
      let func1 = IRFunction "f1" [("p", IRI32)] (Just IRI32) 
            [ IRALLOC "x" IRI32
            , IRASSIGN "x" (IRParam "p" IRI32) IRI32
            , IRRET (Just (IRTemp "x" IRI32))
            ]
          func2 = IRFunction "f2" [] (Just IRNull) [IRRET Nothing]
          prog = IRProgram "complex"
            [ IRExtern "ext"
            , IRGlobalString "g" "str"
            , IRStructDef "S" [("a", IRI32), ("b", IRF32)]
            , IRFunctionDef func1
            , IRFunctionDef func2
            ]
      in runIROptimizer prog @?= prog
  ]
