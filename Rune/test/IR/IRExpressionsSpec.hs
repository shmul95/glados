module IR.IRExpressionsSpec (irExpressionsTests) where

import Control.Monad.State (runState)
import Data.Map (empty)
import qualified Data.Set as Set
import Rune.AST.Nodes (Expression (..))
import Rune.IR.Generator.GenExpression (genExpression)
import Rune.IR.Nodes
  ( GenState (..),
    IRGen,
    IRInstruction (..),
    IROperand (..),
    IRTopLevel (IRGlobalString),
    IRType (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- helpers for testing
--

initialState :: GenState
initialState =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsStringCounter = 0,
      gsGlobals = [],
      gsCurrentFunc = Nothing,
      gsSymTable = empty,
      gsStructs = empty,
      gsLoopStack = [],
      gsCalledFuncs = Set.empty,
      gsStringMap = empty
    }

runIRGen :: IRGen a -> (a, GenState)
runIRGen m = runState m initialState

--
-- public
--

irExpressionsTests :: TestTree
irExpressionsTests =
  testGroup
    "Rune.IR.Literals Specs"
    [ testCase "genLitInt" testGenLitInt,
      testCase "genLitFloat" testGenLitFloat,
      testCase "genLitChar" testGenLitChar,
      testCase "genLitBool True" testGenLitBoolTrue,
      testCase "genLitBool False" testGenLitBoolFalse,
      testCase "genLitNull" testGenLitNull,
      testCase "genLitString" testGenLitString
    ]

--
-- private
--

testGenLitInt :: IO ()
testGenLitInt =
  let expr = ExprLitInt 42
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstInt 42, IRI32)
   in result @?= expected

testGenLitFloat :: IO ()
testGenLitFloat =
  let expr = ExprLitFloat 3.14
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstFloat 3.14, IRF32)
   in result @?= expected

testGenLitChar :: IO ()
testGenLitChar =
  let expr = ExprLitChar 'a'
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstChar 'a', IRChar)
   in result @?= expected

testGenLitBoolTrue :: IO ()
testGenLitBoolTrue =
  let expr = ExprLitBool True
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstBool True, IRBool)
   in result @?= expected

testGenLitBoolFalse :: IO ()
testGenLitBoolFalse =
  let expr = ExprLitBool False
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstBool False, IRBool)
   in result @?= expected

testGenLitNull :: IO ()
testGenLitNull =
  let expr = ExprLitNull
      (result, _) = runIRGen (genExpression expr)
      expected = ([], IRConstNull, IRNull)
   in result @?= expected

testGenLitString :: IO ()
testGenLitString =
  let expr = ExprLitString "hello world"
      (result, finalState) = runIRGen (genExpression expr)

      expectedInstrs = [IRADDR "p_ptr0" "str_global0" (IRPtr IRChar)]
      expectedOperand = IRTemp "p_ptr0" (IRPtr IRChar)
      expectedType = IRPtr IRChar
      expectedGlobals = [IRGlobalString "str_global0" "hello world"]
   in do
        result @?= (expectedInstrs, expectedOperand, expectedType)
        gsGlobals finalState @?= expectedGlobals
        gsStringCounter finalState @?= 1
