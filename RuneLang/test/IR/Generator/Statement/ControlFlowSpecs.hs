{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Statement.ControlFlowSpecs (controlFlowTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=), assertFailure)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import TestHelpers (dummyPos)
import Rune.IR.Generator.Statement.ControlFlow
import Rune.IR.Nodes (IRInstruction(..), IRType(..), IROperand(..), IRLabel(..), GenState(..))
import Rune.AST.Nodes (Expression(..), Statement(..))
import IR.TestUtils (runGenUnsafe, emptyState)

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
      let genExpr (ExprLitBool _ b) = return ([], IRConstBool b, IRBool)
          genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock _ = return []
          instrs = runGenUnsafe (genIfNoElse genExpr genBlock (ExprLitBool dummyPos True) [])
      in do
        assertBool "Should have JUMP_FALSE" $ any isJumpFalse instrs
        assertBool "Should have label" $ any isLabel instrs

  , testCase "Includes then block instructions" $
      let genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock _ = return [IRRET Nothing]
          instrs = runGenUnsafe (genIfNoElse genExpr genBlock (ExprLitBool dummyPos True) [])
      in assertBool "Should have IRRET" $ any isRet instrs
  ]

testGenIfElse :: TestTree
testGenIfElse = testGroup "genIfElse"
  [ testCase "Generates if-else structure" $
      let genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock _ = return []
          instrs = runGenUnsafe (genIfElse genExpr genBlock (ExprLitBool dummyPos True) [] [])
      in do
        assertBool "Should have JUMP_FALSE" $ any isJumpFalse instrs
        assertBool "Should have at least 2 labels" $ length (filter isLabel instrs) >= 2

  , testCase "Skips jump to end if then block ends with return" $
      let genExpr _ = return ([], IRConstBool True, IRBool)
          genBlock stmts =
            case stmts of
              [] -> return []
              _  -> return [IRRET Nothing]
          instrs = runGenUnsafe (genIfElse genExpr genBlock (ExprLitBool dummyPos True) [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))] [StmtReturn dummyPos Nothing])
          hasJumpToEnd = any isJumpToEnd instrs
      in assertBool "Should not emit jump to end when then-branch ends with IRRET" (not hasJumpToEnd)
  ]

testGenStop :: TestTree
testGenStop = testGroup "genStop"
  [ testCase "Generates jump to loop end when in loop" $
      let initialState = emptyState { gsLoopStack = [(IRLabel "Lheader", IRLabel "Lend")] }
      in case runState (runExceptT genStop) initialState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right instrs, _) -> instrs @?= [IRJUMP (IRLabel "Lend")]

  , testCase "Returns empty when not in loop" $
      case runState (runExceptT genStop) emptyState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right instrs, _) -> instrs @?= []
  ]

testGenNext :: TestTree
testGenNext = testGroup "genNext"
  [ testCase "Generates jump to loop header when in loop" $
      let initialState = emptyState { gsLoopStack = [(IRLabel "Lheader", IRLabel "Lend")] }
      in case runState (runExceptT genNext) initialState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right instrs, _) -> instrs @?= [IRJUMP (IRLabel "Lheader")]

  , testCase "Returns empty when not in loop" $
      case runState (runExceptT genNext) emptyState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right instrs, _) -> instrs @?= []
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

isJumpToEnd :: IRInstruction -> Bool
isJumpToEnd (IRJUMP (IRLabel ".L.end0")) = True
isJumpToEnd _ = False
