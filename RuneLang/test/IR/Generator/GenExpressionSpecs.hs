{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

#define TESTING_EXPORT

module IR.Generator.GenExpressionSpecs (genExpressionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestHelpers (dummyPos)
import Control.Monad.State (evalState)
import Control.Exception (evaluate, try, SomeException)
import Rune.IR.Generator.GenExpression
import Rune.IR.Nodes (IRType(..), IROperand(..), GenState(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (emptyState, runGen)
import qualified Data.Map.Strict as Map

--
-- public
--

genExpressionTests :: TestTree
genExpressionTests = testGroup "Rune.IR.Generator.GenExpression"
  [ testGenExpression
  , testGenVar
  ]

--
-- private
--

testGenExpression :: TestTree
testGenExpression = testGroup "genExpression"
  [ testCase "Generates int literal" $
      let (instrs, op, typ) = runGen (genExpression (ExprLitInt dummyPos 42))
      in do
        instrs @?= []
        op @?= IRConstInt 42
        typ @?= IRI32

  , testCase "Generates float literal" $
      let (instrs, op, typ) = runGen (genExpression (ExprLitFloat dummyPos 3.14))
      in do
        instrs @?= []
        op @?= IRGlobal "f32_global0" IRF32
        typ @?= IRF32

  , testCase "Generates char literal" $
      let (instrs, op, typ) = runGen (genExpression (ExprLitChar dummyPos 'a'))
      in do
        instrs @?= []
        op @?= IRConstChar 'a'
        typ @?= IRChar

  , testCase "Generates bool literal" $
      let (instrs, op, typ) = runGen (genExpression (ExprLitBool dummyPos True))
      in do
        instrs @?= []
        op @?= IRConstBool True
        typ @?= IRBool

  , testCase "Generates null literal" $
      let (instrs, op, typ) = runGen (genExpression (ExprLitNull dummyPos))
      in do
        instrs @?= []
        op @?= IRConstNull
        typ @?= IRNull

  , testCase "Generates string literal" $
      let (_, op, typ) = runGen (genExpression (ExprLitString dummyPos "hello"))
      in do
        case op of
          IRGlobal _ (IRPtr IRChar) -> return ()
          _ -> assertBool "Expected IRGlobal" False
        typ @?= IRPtr IRChar
  ]

testGenVar :: TestTree
testGenVar = testGroup "genVar"
  [ testCase "Looks up variable in symbol table" $
      let state = emptyState { gsSymTable = Map.singleton "x" (IRTemp "x" IRI32, IRI32) }
          (instrs, op, typ) = evalState (genVar "x") state
      in do
        instrs @?= []
        op @?= IRTemp "x" IRI32
        typ @?= IRI32

  , testCase "Errors on undefined variable" $
      do
        result <- try @SomeException (evaluate $ runGen (genVar "undefined"))
        case result of
          Left _ -> return ()
          Right _ -> assertBool "Expected error for undefined variable" False
  ]
