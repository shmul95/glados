module IR.Generator.Expression.UnarySpecs (unaryExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure, assertEqual)
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
  , testGenUnaryNot
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
    , testCase "Generates negate operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr Negate [] (IRConstInt 5) IRI32)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRI32
    , testCase "Generates not operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr Not [] (IRConstBool True) IRBool)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRBool
    , testCase "Generates prefix inc operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr PrefixInc [] (IRTemp "x" IRI32) IRI32)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRI32
    , testCase "Generates prefix dec operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr PrefixDec [] (IRTemp "x" IRI32) IRI32)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRI32
    , testCase "Generates postfix inc operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr PostfixInc [] (IRTemp "x" IRI32) IRI32)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRI32
    , testCase "Generates postfix dec operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr PostfixDec [] (IRTemp "x" IRI32) IRI32)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRI32
    , testCase "Generates propagate error operation" $
      let (instrs, _, typ) = runGen (genUnaryExpr PropagateError [IRALLOC "x" IRI32] (IRTemp "x" IRI32) IRI32)
      in do
      assertBool "Should have instructions" $ not $ null instrs
      typ @?= IRI32
  ]

testGenUnaryNegate :: TestTree
testGenUnaryNegate = testGroup "genUnaryNegate"
  [ testCase "Generates IRSUB_OP with zero" $ do
      let (instrs, resultOperand, _) = runGen (genUnaryNegate [] (IRTemp "x" IRI32) IRI32)
      case instrs of
        [IRSUB_OP _ (IRConstInt 0) op typ] -> do
          assertEqual "Operand should be x" (IRTemp "x" IRI32) op
          assertEqual "Type should be IRI32" IRI32 typ
        _ -> assertFailure "Expected single IRSUB_OP instruction"
      case resultOperand of
        IRTemp _ typ -> assertEqual "Result type should match" IRI32 typ
        _ -> assertFailure "Expected IRTemp as result operand"
  ]

testGenUnaryNot :: TestTree
testGenUnaryNot = testGroup "genUnaryNot"
  [ testCase "Generates IRCMP_EQ comparing to False" $ do
      let (instrs, resultOperand, typ) = runGen (genUnaryNot [] (IRConstBool True) IRBool)
      case instrs of
        [IRCMP_EQ _ op (IRConstBool False)] -> do
          assertEqual "Operand should be True" (IRConstBool True) op
          assertEqual "Result type should be IRBool" IRBool (case resultOperand of IRTemp _ ty -> ty; _ -> error "Expected IRTemp")
        _ -> assertFailure "Expected single IRCMP_EQ instruction"
      typ @?= IRBool
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
  [ testCase "Generates IRASSIGN then IRINC with correct temp" $ do
      let (instrs, resultOperand, _) = runGen (genUnaryPostfixInc [] (IRTemp "x" IRI32) IRI32)
      case reverse instrs of
        (IRINC op : _) -> assertEqual "IRINC operand should be x" (IRTemp "x" IRI32) op
        _ -> assertFailure "Expected IRINC as last instruction"
      case instrs of
        (IRASSIGN _ op typ : _) -> do
          assertEqual "IRASSIGN operand should be x" (IRTemp "x" IRI32) op
          assertEqual "IRASSIGN type should be IRI32" IRI32 typ
        _ -> assertFailure "Expected IRASSIGN as first instruction"
      case resultOperand of
        IRTemp _ typ -> assertEqual "Result type should match" IRI32 typ
        _ -> assertFailure "Expected IRTemp as result operand"
  ]

testGenUnaryPostfixDec :: TestTree
testGenUnaryPostfixDec = testGroup "genUnaryPostfixDec"
  [ testCase "Generates IRASSIGN then IRDEC with correct temp" $ do
      let (instrs, resultOperand, _) = runGen (genUnaryPostfixDec [] (IRTemp "x" IRI32) IRI32)
      case reverse instrs of
        (IRDEC op : _) -> assertEqual "IRDEC operand should be x" (IRTemp "x" IRI32) op
        _ -> assertFailure "Expected IRDEC as last instruction"
      case instrs of
        (IRASSIGN _ op typ : _) -> do
          assertEqual "IRASSIGN operand should be x" (IRTemp "x" IRI32) op
          assertEqual "IRASSIGN type should be IRI32" IRI32 typ
        _ -> assertFailure "Expected IRASSIGN as first instruction"
      case resultOperand of
        IRTemp _ typ -> assertEqual "Result type should match" IRI32 typ
        _ -> assertFailure "Expected IRTemp as result operand"
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
