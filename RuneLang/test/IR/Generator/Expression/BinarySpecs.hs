{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.BinarySpecs (binaryExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Binary
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..))
import Rune.AST.Nodes (BinaryOp(..), Expression(..))
import IR.TestUtils (runGen)

--
-- public
--

binaryExprTests :: TestTree
binaryExprTests = testGroup "Rune.IR.Generator.Expression.Binary"
  [ testGenBinary
  , testGetResultType
  , testMkInstr
  ]

--
-- private
--

testGenBinary :: TestTree
testGenBinary = testGroup "genBinary"
  [ testCase "Generates ADD operation" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, _, typ) = runGen (genBinary genExpr Add (ExprLitInt 1) (ExprLitInt 2))
      in do
        assertBool "Should have ADD instruction" $ not $ null instrs
        case last instrs of
          IRADD_OP _ _ _ _ -> return ()
          _ -> assertBool "Expected IRADD_OP" False
        typ @?= IRI32

  , testCase "Generates comparison operation" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, _, typ) = runGen (genBinary genExpr Eq (ExprLitInt 1) (ExprLitInt 2))
      in do
        typ @?= IRBool
        case last instrs of
          IRCMP_EQ _ _ _ -> return ()
          _ -> assertBool "Expected IRCMP_EQ" False
  ]

testGetResultType :: TestTree
testGetResultType = testGroup "getResultType"
  [ testCase "Comparison ops return IRBool" $ do
      getResultType Eq IRI32 @?= IRBool
      getResultType Neq IRI32 @?= IRBool
      getResultType Lt IRI32 @?= IRBool
      getResultType Lte IRI32 @?= IRBool
      getResultType Gt IRI32 @?= IRBool
      getResultType Gte IRI32 @?= IRBool

  , testCase "Logical ops return IRBool" $ do
      getResultType And IRI32 @?= IRBool
      getResultType Or IRI32 @?= IRBool

  , testCase "Arithmetic ops preserve type" $ do
      getResultType Add IRI32 @?= IRI32
      getResultType Sub IRF64 @?= IRF64
      getResultType Mul IRI64 @?= IRI64
      getResultType Div IRF32 @?= IRF32
      getResultType Mod IRI32 @?= IRI32
  ]

testMkInstr :: TestTree
testMkInstr = testGroup "mkInstr"
  [ testCase "Creates IRADD_OP for Add" $
      case mkInstr Add "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRADD_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IRADD_OP" False

  , testCase "Creates IRSUB_OP for Sub" $
      case mkInstr Sub "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRSUB_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IRSUB_OP" False

  , testCase "Creates IRMUL_OP for Mul" $
      case mkInstr Mul "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRMUL_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IRMUL_OP" False

  , testCase "Creates IRDIV_OP for Div" $
      case mkInstr Div "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRDIV_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IRDIV_OP" False

  , testCase "Creates IRMOD_OP for Mod" $
      case mkInstr Mod "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRMOD_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IRMOD_OP" False

  , testCase "Creates IRCMP_EQ for Eq" $
      case mkInstr Eq "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRCMP_EQ _ _ _ -> return ()
        _ -> assertBool "Expected IRCMP_EQ" False

  , testCase "Creates IRCMP_NEQ for Neq" $
      case mkInstr Neq "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRCMP_NEQ _ _ _ -> return ()
        _ -> assertBool "Expected IRCMP_NEQ" False

  , testCase "Creates IRCMP_LT for Lt" $
      case mkInstr Lt "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRCMP_LT _ _ _ -> return ()
        _ -> assertBool "Expected IRCMP_LT" False

  , testCase "Creates IRCMP_LTE for Lte" $
      case mkInstr Lte "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRCMP_LTE _ _ _ -> return ()
        _ -> assertBool "Expected IRCMP_LTE" False

  , testCase "Creates IRCMP_GT for Gt" $
      case mkInstr Gt "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRCMP_GT _ _ _ -> return ()
        _ -> assertBool "Expected IRCMP_GT" False

  , testCase "Creates IRCMP_GTE for Gte" $
      case mkInstr Gte "t" (IRConstInt 1) (IRConstInt 2) IRI32 of
        IRCMP_GTE _ _ _ -> return ()
        _ -> assertBool "Expected IRCMP_GTE" False

  , testCase "Creates IRAND_OP for And" $
      case mkInstr And "t" (IRConstBool True) (IRConstBool False) IRBool of
        IRAND_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IRAND_OP" False

  , testCase "Creates IROR_OP for Or" $
      case mkInstr Or "t" (IRConstBool True) (IRConstBool False) IRBool of
        IROR_OP _ _ _ _ -> return ()
        _ -> assertBool "Expected IROR_OP" False
  ]
