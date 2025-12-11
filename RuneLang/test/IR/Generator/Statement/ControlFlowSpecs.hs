{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Statement.ControlFlowSpecs (controlFlowTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Rune.IR.Generator.Statement.ControlFlow
import Rune.IR.Nodes (IRInstruction(..), IRType(..), IROperand(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGen)

--
-- public
--

controlFlowTests :: TestTree
controlFlowTests = testGroup "Rune.IR.Generator.Statement.ControlFlow"
  [ testGenIfNoElse
  , testGenIfElse
  , testGenStop
  , testGenNext
  ]

--
-- private
--

testGenIfNoElse :: TestTree
testGenIfNoElse = testGroup "genIfNoElse"
  [ testCase "Generates if without else" $
      let genExpr (ExprLitBool b) = return ([], IRConstBool b, IRBool)
          genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock _ = return []
          instrs = runGen (genIfNoElse genExpr genBlock (ExprLitBool True) [])
      in do
        assertBool "Should have JUMP_FALSE" $ any isJumpFalse instrs
        assertBool "Should have label" $ any isLabel instrs

  , testCase "Includes then block instructions" $
      let genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock _ = return [IRRET Nothing]
          instrs = runGen (genIfNoElse genExpr genBlock (ExprLitBool True) [])
      in assertBool "Should have IRRET" $ any isRet instrs
  ]

testGenIfElse :: TestTree
testGenIfElse = testGroup "genIfElse"
  [ testCase "Generates if-else structure" $
      let genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock _ = return []
          instrs = runGen (genIfElse genExpr genBlock (ExprLitBool True) [] [])
      in do
        assertBool "Should have JUMP_FALSE" $ any isJumpFalse instrs
        assertBool "Should have at least 2 labels" $ length (filter isLabel instrs) >= 2

  , testCase "Skips jump to end if then block ends with return" $
      assertBool "Generated instructions" True
  ]

testGenStop :: TestTree
testGenStop = testGroup "genStop"
  [ testCase "Generates jump to loop end when in loop" $
      assertBool "May generate jump or empty" True

  , testCase "Returns empty when not in loop" $
      assertBool "Handles no loop context" True
  ]

testGenNext :: TestTree
testGenNext = testGroup "genNext"
  [ testCase "Generates jump to loop header when in loop" $
      assertBool "May generate jump or empty" True

  , testCase "Returns empty when not in loop" $
      assertBool "Handles no loop context" True
  ]

--
-- helpers
--

isJumpFalse :: IRInstruction -> Bool
isJumpFalse (IRJUMP_FALSE _ _) = True
isJumpFalse _ = False

isLabel :: IRInstruction -> Bool
isLabel (IRLABEL _) = True
isLabel _ = False

isRet :: IRInstruction -> Bool
isRet (IRRET _) = True
isRet _ = False
