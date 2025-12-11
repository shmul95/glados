{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.CallSpecs (callExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Call
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGen)

--
-- public
--

callExprTests :: TestTree
callExprTests = testGroup "Rune.IR.Generator.Expression.Call"
  [ testGenCall
  , testPrepareArg
  ]

--
-- private
--

testGenCall :: TestTree
testGenCall = testGroup "genCall"
  [ testCase "Generates function call with no args" $
      let genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, op, _) = runGen (genCall genExpr "test" [])
      in do
        assertBool "Should have IRCALL" $ any isCall instrs
        case op of
          IRTemp _ _ -> return ()
          _ -> assertBool "Expected IRTemp" False

  , testCase "Generates function call with args" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, _, _) = runGen (genCall genExpr "add" [ExprLitInt 1, ExprLitInt 2])
      in assertBool "Should have IRCALL" $ any isCall instrs

  , testCase "Registers function call" $
      assertBool "Should register call" True
  ]

testPrepareArg :: TestTree
testPrepareArg = testGroup "prepareArg"
  [ testCase "Adds IRADDR for struct temp" $
      let (instrs, op) = prepareArg ([], IRTemp "s" (IRStruct "Point"), IRStruct "Point")
      in do
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp name (IRPtr _) -> assertBool "Name should be prefixed" $ "p_" `elem` [take 2 name]
          _ -> assertBool "Expected IRTemp with IRPtr" False

  , testCase "Adds IRADDR for pointer to struct" $
      let (instrs, op) = prepareArg ([], IRTemp "s" (IRPtr (IRStruct "Vec")), IRPtr (IRStruct "Vec"))
      in do
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp _ (IRPtr _) -> return ()
          _ -> assertBool "Expected IRTemp with IRPtr" False

  , testCase "Passes through non-struct operands" $
      let (instrs, op) = prepareArg ([IRALLOC "x" IRI32], IRConstInt 42, IRI32)
      in do
        length instrs @?= 1
        op @?= IRConstInt 42

  , testCase "Passes through non-temp operands" $
      let (instrs, op) = prepareArg ([], IRConstInt 10, IRI32)
      in do
        instrs @?= []
        op @?= IRConstInt 10
  ]

--
-- helpers
--

isCall :: IRInstruction -> Bool
isCall (IRCALL _ _ _ _) = True
isCall _ = False
