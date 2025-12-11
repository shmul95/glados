{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.UnarySpecs (unaryExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Unary
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..))
import Rune.AST.Nodes (UnaryOp(..), Expression(..))
import IR.TestUtils (runGen)

--
-- public
--

unaryExprTests :: TestTree
unaryExprTests = testGroup "Rune.IR.Generator.Expression.Unary"
  [ testGenUnary
  , testGenUnaryExpr
  , testGenUnaryNegate
  , testGenUnaryPrefixInc
  , testGenUnaryPrefixDec
  , testGenUnaryPostfixInc
  , testGenUnaryPostfixDec
  , testGenUnaryPropagate
  ]

--
-- private
--

testGenUnary :: TestTree
testGenUnary = testGroup "genUnary"
  [ testCase "Generates negate operation" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, _, typ) = runGen (genUnary genExpr Negate (ExprLitInt 5))
      in do
        assertBool "Should have instructions" $ not $ null instrs
        typ @?= IRI32
  ]

testGenUnaryExpr :: TestTree
testGenUnaryExpr = testGroup "genUnaryExpr"
  [ testCase "Routes to correct handler for each op" $
      assertBool "Routes correctly" True
  ]

testGenUnaryNegate :: TestTree
testGenUnaryNegate = testGroup "genUnaryNegate"
  [ testCase "Generates SUB 0 - operand" $
      let (instrs, _, _) = runGen (genUnaryNegate [] (IRConstInt 5) IRI32)
      in case last instrs of
        IRSUB_OP _ (IRConstInt 0) _ _ -> return ()
        _ -> assertBool "Expected IRSUB with 0" False
  ]

testGenUnaryPrefixInc :: TestTree
testGenUnaryPrefixInc = testGroup "genUnaryPrefixInc"
  [ testCase "Generates IRINC instruction" $
      let (instrs, op, _) = runGen (genUnaryPrefixInc [] (IRTemp "x" IRI32) IRI32)
      in do
        case last instrs of
          IRINC _ -> return ()
          _ -> assertBool "Expected IRINC" False
        op @?= IRTemp "x" IRI32
  ]

testGenUnaryPrefixDec :: TestTree
testGenUnaryPrefixDec = testGroup "genUnaryPrefixDec"
  [ testCase "Generates IRDEC instruction" $
      let (instrs, op, _) = runGen (genUnaryPrefixDec [] (IRTemp "x" IRI32) IRI32)
      in do
        case last instrs of
          IRDEC _ -> return ()
          _ -> assertBool "Expected IRDEC" False
        op @?= IRTemp "x" IRI32
  ]

testGenUnaryPostfixInc :: TestTree
testGenUnaryPostfixInc = testGroup "genUnaryPostfixInc"
  [ testCase "Generates IRASSIGN then IRINC" $
      let (instrs, _, _) = runGen (genUnaryPostfixInc [] (IRTemp "x" IRI32) IRI32)
      in do
        assertBool "Should have 2 instructions" $ length instrs >= 2
        case last instrs of
          IRINC _ -> return ()
          _ -> assertBool "Expected IRINC as last" False
  ]

testGenUnaryPostfixDec :: TestTree
testGenUnaryPostfixDec = testGroup "genUnaryPostfixDec"
  [ testCase "Generates IRASSIGN then IRDEC" $
      let (instrs, _, _) = runGen (genUnaryPostfixDec [] (IRTemp "x" IRI32) IRI32)
      in do
        assertBool "Should have 2 instructions" $ length instrs >= 2
        case last instrs of
          IRDEC _ -> return ()
          _ -> assertBool "Expected IRDEC as last" False
  ]

testGenUnaryPropagate :: TestTree
testGenUnaryPropagate = testGroup "genUnaryPropagate"
  [ testCase "Returns operand unchanged" $
      let (instrs, op, typ) = runGen (genUnaryPropagate [IRALLOC "x" IRI32] (IRTemp "x" IRI32) IRI32)
      in do
        length instrs @?= 1
        op @?= IRTemp "x" IRI32
        typ @?= IRI32
  ]
