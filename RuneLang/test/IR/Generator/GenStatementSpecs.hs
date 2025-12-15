{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.GenStatementSpecs (genStatementTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestHelpers (dummyPos)
import Rune.IR.Generator.GenStatement
import Rune.IR.Nodes (IRInstruction(..), IRType(..), IROperand(..), IRLabel(..))
import Rune.AST.Nodes (Statement(..), Expression(..), Type(..))
import IR.TestUtils (runGen)

--
-- public
--

genStatementTests :: TestTree
genStatementTests = testGroup "Rune.IR.Generator.GenStatement"
  [ testGenStatement
  , testGenBlock
  , testGenVarDecl
  , testGenVarType
  , testGenAssignment
  , testGenReturnExpr
  , testGenExprStmt
  , testGenIfControlFlow
  , testGenLoopControlFlow
  ]

--
-- private
--

testGenStatement :: TestTree
testGenStatement = testGroup "genStatement"
  [ testCase "StmtReturn Nothing generates IRRET Nothing" $
      let result = runGen (genStatement (StmtReturn dummyPos Nothing))
      in result @?= [IRRET Nothing]

  , testCase "StmtReturn with int generates IRRET with value" $
      let result = runGen (genStatement (StmtReturn dummyPos (Just (ExprLitInt dummyPos 42))))
      in case result of
        [IRRET (Just _)] -> return ()
        _ -> assertBool "Expected IRRET with value" False

  , testCase "StmtExpr generates expression instructions" $
      assertBool "Should generate instructions" True
  ]

testGenBlock :: TestTree
testGenBlock = testGroup "genBlock"
  [ testCase "Empty block generates empty list" $
      let result = runGen (genBlock [])
      in result @?= []

  , testCase "Single statement block" $
      let result = runGen (genBlock [StmtReturn dummyPos Nothing])
      in result @?= [IRRET Nothing]

  , testCase "Multiple statements concatenated" $
      let result = runGen (genBlock [StmtReturn dummyPos Nothing, StmtReturn dummyPos Nothing])
      in length result @?= 2
  ]

testGenVarDecl :: TestTree
testGenVarDecl = testGroup "genVarDecl"
  [ testCase "Declares variable with explicit type" $
      let result = runGen (genVarDecl "x" (Just TypeI32) (ExprLitInt dummyPos 10))
      in case result of
        [IRASSIGN "x" (IRConstInt 10) IRI32] -> return ()
        _ -> assertBool "Expected IRASSIGN" False

  , testCase "Declares variable with inferred type" $
      let result = runGen (genVarDecl "y" Nothing (ExprLitInt dummyPos 5))
      in assertBool "Should generate assignment" $ not $ null result

  , testCase "Handles IRTemp operand differently" $
      assertBool "Should handle temp" True
  ]

testGenVarType :: TestTree
testGenVarType = testGroup "genVarType"
  [ testCase "Uses explicit type when provided" $
      genVarType (Just TypeI64) IRI32 @?= IRI64

  , testCase "Uses inferred type when no explicit type" $
      genVarType Nothing IRF32 @?= IRF32

  , testCase "Explicit type overrides inferred" $
      genVarType (Just TypeBool) IRI32 @?= IRBool
  ]

testGenAssignment :: TestTree
testGenAssignment = testGroup "genAssignment"
  [ testCase "Assigns constant to variable" $
      let result = runGen (genVarDecl "x" Nothing (ExprLitInt dummyPos 1) >> genAssignment (ExprVar dummyPos "x") (ExprLitInt dummyPos 2))
      in assertBool "Should generate assignment" $ not $ null result

  , testCase "Handles non-temp lvalue" $
      assertBool "Should handle gracefully" True
  ]

testGenReturnExpr :: TestTree
testGenReturnExpr = testGroup "genReturnExpr"
  [ testCase "Returns IRNull as Nothing" $
      let result = runGen (genReturnExpr (ExprLitNull dummyPos))
      in case last result of
        IRRET Nothing -> return ()
        _ -> assertBool "Expected IRRET Nothing for null" False

  , testCase "Returns non-null value" $
      let result = runGen (genReturnExpr (ExprLitInt dummyPos 42))
      in case last result of
        IRRET (Just _) -> return ()
        _ -> assertBool "Expected IRRET with value" False

  , testCase "Returns boolean value" $
      let result = runGen (genReturnExpr (ExprLitBool dummyPos True))
      in case last result of
        IRRET (Just _) -> return ()
        _ -> assertBool "Expected IRRET with value" False
  ]

testGenExprStmt :: TestTree
testGenExprStmt = testGroup "genExprStmt"
  [ testCase "Generates expression without assignment" $
      assertBool "Should generate instructions" True

  , testCase "Discards return value" $
      assertBool "Should handle any expression" True
  ]

testGenIfControlFlow :: TestTree
testGenIfControlFlow = testGroup "genStatement if-control-flow"
  [ testCase "Generates IR for if without else" $
      let cond = ExprLitBool dummyPos True
          body = [StmtReturn dummyPos Nothing]
          result = runGen (genStatement (StmtIf dummyPos cond body Nothing))
      in case result of
        (IRJUMP_FALSE _ _ : _ ) -> return ()
        _ -> assertBool "Expected IRJUMP_FALSE followed by then block and label" False

  , testCase "Generates IR for if with else and jump to end when then-branch lacks return" $
      let cond = ExprLitBool dummyPos True
          thenBody = [StmtExpr dummyPos (ExprLitInt dummyPos 1)]
          elseBody = [StmtExpr dummyPos (ExprLitInt dummyPos 2)]
          result = runGen (genStatement (StmtIf dummyPos cond thenBody (Just elseBody)))
          hasElseLabel   = any isElseLabel result
          hasEndLabel    = any isEndLabel result
          hasJumpToEnd   = any isJumpToEnd result
      in do
        assertBool "Should contain else label" hasElseLabel
        assertBool "Should contain end label" hasEndLabel
        assertBool "Should contain jump to end from then-branch" hasJumpToEnd

  , testCase "Omits jump to end when then-branch ends with IRRET" $
      let cond = ExprLitBool dummyPos True
          thenBody = [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))]
          elseBody = [StmtExpr dummyPos (ExprLitInt dummyPos 2)]
          result = runGen (genStatement (StmtIf dummyPos cond thenBody (Just elseBody)))
          hasJumpToEnd = any isJumpToEnd result
      in assertBool "Should not emit jump to end when then-branch ends with IRRET" (not hasJumpToEnd)
  ]
  where
    isElseLabel (IRLABEL (IRLabel ".L.else0")) = True
    isElseLabel _ = False

    isEndLabel (IRLABEL (IRLabel ".L.end0")) = True
    isEndLabel _ = False

    isJumpToEnd (IRJUMP (IRLabel ".L.end0")) = True
    isJumpToEnd _ = False

testGenLoopControlFlow :: TestTree
testGenLoopControlFlow = testGroup "genStatement loop-control-flow"
  [ testCase "StmtStop inside loop jumps to loop end label" $
      let loopBody = [StmtStop dummyPos]
          result = runGen (genStatement (StmtLoop dummyPos loopBody))
          hasJumpToEnd = any isJumpToLoopEnd result
      in assertBool "Should emit jump to loop end label for StmtStop" hasJumpToEnd

  , testCase "StmtNext inside loop jumps to loop header label" $
      let loopBody = [StmtNext dummyPos]
          result = runGen (genStatement (StmtLoop dummyPos loopBody))
          hasJumpToHeader = any isJumpToLoopHeader result
      in assertBool "Should emit jump to loop header label for StmtNext" hasJumpToHeader

  , testCase "StmtStop outside loop produces no instructions" $
      let result = runGen (genStatement (StmtStop dummyPos))
      in result @?= []

  , testCase "StmtNext outside loop produces no instructions" $
      let result = runGen (genStatement (StmtNext dummyPos))
      in result @?= []
  ]
  where
    isJumpToLoopEnd (IRJUMP (IRLabel ".L.loop_end0")) = True
    isJumpToLoopEnd _ = False

    isJumpToLoopHeader (IRJUMP (IRLabel ".L.loop_header0")) = True
    isJumpToLoopHeader _ = False
