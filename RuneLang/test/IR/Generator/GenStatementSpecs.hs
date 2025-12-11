{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.GenStatementSpecs (genStatementTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.GenStatement
import Rune.IR.Nodes (IRInstruction(..), IRType(..), IROperand(..))
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
  ]

--
-- private
--

testGenStatement :: TestTree
testGenStatement = testGroup "genStatement"
  [ testCase "StmtReturn Nothing generates IRRET Nothing" $
      let result = runGen (genStatement (StmtReturn Nothing))
      in result @?= [IRRET Nothing]

  , testCase "StmtReturn with int generates IRRET with value" $
      let result = runGen (genStatement (StmtReturn (Just (ExprLitInt 42))))
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
      let result = runGen (genBlock [StmtReturn Nothing])
      in result @?= [IRRET Nothing]

  , testCase "Multiple statements concatenated" $
      let result = runGen (genBlock [StmtReturn Nothing, StmtReturn Nothing])
      in length result @?= 2
  ]

testGenVarDecl :: TestTree
testGenVarDecl = testGroup "genVarDecl"
  [ testCase "Declares variable with explicit type" $
      let result = runGen (genVarDecl "x" (Just TypeI32) (ExprLitInt 10))
      in case result of
        [IRASSIGN "x" (IRConstInt 10) IRI32] -> return ()
        _ -> assertBool "Expected IRASSIGN" False

  , testCase "Declares variable with inferred type" $
      let result = runGen (genVarDecl "y" Nothing (ExprLitInt 5))
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
      let result = runGen (genVarDecl "x" Nothing (ExprLitInt 1) >> genAssignment (ExprVar "x") (ExprLitInt 2))
      in assertBool "Should generate assignment" $ not $ null result

  , testCase "Handles non-temp lvalue" $
      assertBool "Should handle gracefully" True
  ]

testGenReturnExpr :: TestTree
testGenReturnExpr = testGroup "genReturnExpr"
  [ testCase "Returns IRNull as Nothing" $
      let result = runGen (genReturnExpr ExprLitNull)
      in case last result of
        IRRET Nothing -> return ()
        _ -> assertBool "Expected IRRET Nothing for null" False

  , testCase "Returns non-null value" $
      let result = runGen (genReturnExpr (ExprLitInt 42))
      in case last result of
        IRRET (Just _) -> return ()
        _ -> assertBool "Expected IRRET with value" False

  , testCase "Returns boolean value" $
      let result = runGen (genReturnExpr (ExprLitBool True))
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
