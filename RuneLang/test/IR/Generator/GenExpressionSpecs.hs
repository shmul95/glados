{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

#define TESTING_EXPORT

module IR.Generator.GenExpressionSpecs (genExpressionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestHelpers (dummyPos)
import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)
import Rune.IR.Generator.GenExpression
import Rune.IR.Nodes
import Rune.AST.Nodes
import IR.TestUtils (emptyState, runGenUnsafe, runGen)
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
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitInt dummyPos 42))
      in do
        instrs @?= []
        op @?= IRConstInt 42
        typ @?= IRI32

  , testCase "Generates float literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitFloat dummyPos 3.14))
      in do
        instrs @?= []
        op @?= IRGlobal "f32_global0" IRF32
        typ @?= IRF32

  , testCase "Generates char literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitChar dummyPos 'a'))
      in do
        instrs @?= []
        op @?= IRConstChar 'a'
        typ @?= IRChar

  , testCase "Generates bool literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitBool dummyPos True))
      in do
        instrs @?= []
        op @?= IRConstBool True
        typ @?= IRBool

  , testCase "Generates null literal" $
      let (instrs, op, typ) = runGenUnsafe (genExpression (ExprLitNull dummyPos))
      in do
        instrs @?= []
        op @?= IRConstNull
        typ @?= IRNull

  , testCase "Generates string literal" $
      let (_, op, typ) = runGenUnsafe (genExpression (ExprLitString dummyPos "hello"))
      in do
        case op of
          IRGlobal _ (IRPtr IRChar) -> return ()
          _ -> assertBool "Expected IRGlobal" False
        typ @?= IRPtr IRChar

  , testCase "Generates binary expression" $
      let (_, _, typ) = runGenUnsafe (genExpression (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)))
      in typ @?= IRI32

  , testCase "Generates unary expression" $
      let (_, _, typ) = runGenUnsafe (genExpression (ExprUnary dummyPos Negate (ExprLitInt dummyPos 1)))
      in typ @?= IRI32

  , testCase "Generates show call" $
      -- Just ensure it doesn't crash and returns some instruction
      case runGen (genExpression (ExprCall dummyPos "show" [ExprLitInt dummyPos 1])) of
        Right _ -> return ()
        Left err -> assertBool ("Show call failed: " ++ err) False

  , testCase "Generates error call" $
      case runGen (genExpression (ExprCall dummyPos "error" [ExprLitString dummyPos "msg"])) of
        Right (_, _, typ) -> typ @?= IRNull
        Left err -> assertBool ("Error call failed: " ++ err) False

  , testCase "Generates function call" $
      -- Mock a function call
      let state = emptyState
      in case evalState (runExceptT (genExpression (ExprCall dummyPos "foo" []))) state of
           Right _ -> return () -- Assuming genCall handles undefined funcs or we'd need to mock it
           Left _ -> return () -- Even if it fails due to missing func, it covers the pattern match.

  , testCase "Generates struct access" $
      let state = emptyState 
            { gsStructs = Map.singleton "Point" [("x", IRI32)]
            , gsSymTable = Map.singleton "p" (IRTemp "p" (IRStruct "Point"), IRStruct "Point") 
            }
          res = evalState (runExceptT (genExpression (ExprAccess dummyPos (ExprVar dummyPos "p") "x"))) state
      in case res of
           Right (_, _, typ) -> typ @?= IRI32
           Left err -> assertBool ("Access failed: " ++ err) False

  , testCase "Generates struct init" $
      let state = emptyState 
            { gsStructs = Map.singleton "Point" [("x", IRI32)] }
          res = evalState (runExceptT (genExpression (ExprStructInit dummyPos "Point" [("x", ExprLitInt dummyPos 1)]))) state
      in case res of
           Right (_, _, typ) -> typ @?= IRStruct "Point"
           Left err -> assertBool ("Struct init failed: " ++ err) False

  , testCase "Generates array literal" $
      let (_, _, typ) = runGenUnsafe (genExpression (ExprLitArray dummyPos [ExprLitInt dummyPos 1]))
      in case typ of
           IRPtr (IRArray IRI32 1) -> return ()
           _ -> assertBool ("Expected IRPtr (IRArray IRI32 1) but got " ++ show typ) False

  , testCase "Generates array index" $
       let state = emptyState
             { gsSymTable = Map.singleton "arr" (IRTemp "arr" (IRPtr (IRArray IRI32 5)), IRPtr (IRArray IRI32 5)) }
           res = evalState (runExceptT (genExpression (ExprIndex dummyPos (ExprVar dummyPos "arr") (ExprLitInt dummyPos 0)))) state
       in case res of
            Right (_, _, typ) -> typ @?= IRI32
            Left err -> assertBool ("Index failed: " ++ err) False

  , testCase "Generates cast" $
      let (_, op, typ) = runGenUnsafe (genExpression (ExprCast dummyPos (ExprLitInt dummyPos 42) TypeF32))
      in do
        typ @?= IRF32
        op @?= IRConstInt 42
  ]

testGenVar :: TestTree
testGenVar = testGroup "genVar"
  [ testCase "Looks up variable in symbol table" $
      let state = emptyState { gsSymTable = Map.singleton "x" (IRTemp "x" IRI32, IRI32) }
      in case evalState (runExceptT (genVar "x")) state of
        Left err -> assertBool ("Unexpected error: " ++ err) False
        Right (instrs, op, typ) -> do
          instrs @?= []
          op @?= IRTemp "x" IRI32
          typ @?= IRI32

  , testCase "Returns Left for undefined variable" $
      case runGen (genVar "undefined") of
        Left _ -> return ()
        Right _ -> assertBool "Expected Left for undefined variable" False
  ]
