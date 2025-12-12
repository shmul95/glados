{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Statement.ControlFlowSpecs (controlFlowTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import Control.Monad.State (runState)
import Rune.IR.Generator.Statement.ControlFlow
import Rune.IR.Nodes (IRInstruction(..), IRType(..), IROperand(..), IRLabel(..), GenState(..))
import Rune.AST.Nodes (Expression(..), Statement(..))
import IR.TestUtils (runGen, emptyState)

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
      -- explanation
      -- When the then-branch ends with IRRET, genIfElse must not emit the extra IRJUMP to the shared end label
      let genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock stmts =
            case stmts of
              [] -> return []
              _  -> return [IRRET Nothing]
          instrs = runGen (genIfElse genExpr genBlock (ExprLitBool True) [StmtReturn (Just (ExprLitInt 1))] [StmtReturn Nothing])
          hasJumpToEnd = any isJumpToEnd instrs
      in assertBool "Should not emit jump to end when then-branch ends with IRRET" (not hasJumpToEnd)
      -- old code commented out
      -- , testCase "Skips jump to end if then block ends with return" $
      --     assertBool "Generated instructions" True
  ]

testGenStop :: TestTree
testGenStop = testGroup "genStop"
  [ testCase "Generates jump to loop end when in loop" $
      -- explanation
      -- With a non-empty loop context, genStop should jump to the recorded loop end label
      let initialState = emptyState { gsLoopStack = [(IRLabel "Lheader", IRLabel "Lend")] }
          (instrs, _) = runState genStop initialState
      in instrs @?= [IRJUMP (IRLabel "Lend")]

  , testCase "Returns empty when not in loop" $
      -- explanation
      -- With no loop context, genStop must not emit any instructions
      let (instrs, _) = runState genStop emptyState
      in instrs @?= []
  ]

testGenNext :: TestTree
testGenNext = testGroup "genNext"
  [ testCase "Generates jump to loop header when in loop" $
      -- explanation
      -- With a non-empty loop context, genNext should jump to the recorded loop header label
      let initialState = emptyState { gsLoopStack = [(IRLabel "Lheader", IRLabel "Lend")] }
          (instrs, _) = runState genNext initialState
      in instrs @?= [IRJUMP (IRLabel "Lheader")]

  , testCase "Returns empty when not in loop" $
      -- explanation
      -- With no loop context, genNext must not emit any instructions
      let (instrs, _) = runState genNext emptyState
      in instrs @?= []
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

-- explanation
-- Helper to detect jumps to the synthetic end label emitted by genIfElse for then-branches without a final return
isJumpToEnd :: IRInstruction -> Bool
isJumpToEnd (IRJUMP (IRLabel ".L.end0")) = True
isJumpToEnd _ = False
-- old code commented out
-- isJumpToEnd :: IRInstruction -> Bool
-- isJumpToEnd _ = False
