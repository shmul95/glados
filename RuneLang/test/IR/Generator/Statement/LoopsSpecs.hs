{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Statement.LoopsSpecs (loopsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestHelpers (dummyPos)
import Rune.IR.Generator.Statement.Loops
import Rune.IR.Nodes (IRInstruction(..), IRType(..), IROperand(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGenUnsafe)

--
-- public
--

loopsTests :: TestTree
loopsTests = testGroup "Rune.IR.Generator.Statement.Loops"
  [ testGenForTo
  , testGenForEach
  , testGenLoop
  ]

--
-- private
--

testGenForTo :: TestTree
testGenForTo = testGroup "genForTo"
  [ testCase "Generates for loop with start" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          genBlock _ = return []
          instrs = runGenUnsafe (genForTo genExpr genBlock "i" Nothing (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10) [])
      in do
        assertBool "Should have labels" $ any isLabel instrs
        assertBool "Should have comparison" $ any isCmp instrs
        length (filter isLabel instrs) @?= 3

  , testCase "Generates for loop without start (defaults to 0)" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          genBlock _ = return []
          instrs = runGenUnsafe (genForTo genExpr genBlock "i" Nothing Nothing (ExprLitInt dummyPos 10) [])
      in assertBool "Should generate loop" $ not $ null instrs
  ]

testGenForEach :: TestTree
testGenForEach = testGroup "genForEach"
  [ testCase "Generates foreach loop" $
      let genExpr (ExprLitString _ s) = return ([], IRGlobal s (IRPtr IRChar), IRPtr IRChar)
          genExpr _ = return ([], IRConstNull, IRNull)
          genBlock _ = return []
          instrs = runGenUnsafe (genForEach genExpr genBlock "ch" (ExprLitString dummyPos "str") [])
      in do
        assertBool "Should have labels" $ any isLabel instrs
        assertBool "Should have IRDEREF" $ any isDeref instrs
        assertBool "Should have IRJUMP_EQ0" $ any isJumpEq0 instrs
  ]

testGenLoop :: TestTree
testGenLoop = testGroup "genLoop"
  [ testCase "Generates infinite loop" $
      let genBlock _ = return []
          instrs = runGenUnsafe (genLoop genBlock [])
      in do
        assertBool "Should have header label" $ any isLabel instrs
        assertBool "Should have jump back" $ any isJump instrs
        length (filter isLabel instrs) @?= 2
  ]

--
-- helpers
--

isLabel :: IRInstruction -> Bool
isLabel (IRLABEL _) = True
isLabel _ = False

isCmp :: IRInstruction -> Bool
isCmp (IRCMP_LT _ _ _) = True
isCmp _ = False

isDeref :: IRInstruction -> Bool
isDeref (IRDEREF _ _ _) = True
isDeref _ = False

isJumpEq0 :: IRInstruction -> Bool
isJumpEq0 (IRJUMP_EQ0 _ _) = True
isJumpEq0 _ = False

isJump :: IRInstruction -> Bool
isJump (IRJUMP _) = True
isJump _ = False
