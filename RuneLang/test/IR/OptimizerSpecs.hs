module IR.OptimizerSpecs (optimizerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.IR.Optimizer (runIROptimizer)
import Rune.IR.Nodes (IRProgram(..))

--
-- public
--

optimizerTests :: TestTree
optimizerTests = testGroup "Rune.IR.Optimizer"
  [ testCase "Optimizer returns program unchanged (identity)" $
      let prog = IRProgram "test" []
      in runIROptimizer prog @?= prog
  ]
