{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.ArraySpecs (arrayExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import Data.List (isInfixOf)
import Rune.IR.Generator.Expression.Array
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..), IRGen)
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGenUnsafe, emptyState)
import TestHelpers (dummyPos)

--
-- public
--

arrayExprTests :: TestTree
arrayExprTests = testGroup "Rune.IR.Generator.Expression.Array"
  [ testGenLitArray
  , testGenIndex
  , testGenIndexAssign
  ]

--
-- private
--

-- Mock generator
genExprMock :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExprMock (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
genExprMock (ExprLitBool _ b) = return ([], IRConstBool b, IRBool)
genExprMock (ExprVar _ "arr") = return ([], IRTemp "arr" (IRPtr (IRArray IRI32 5)), IRPtr (IRArray IRI32 5))
genExprMock (ExprVar _ "not_arr") = return ([], IRConstInt 0, IRI32)
genExprMock e = error $ "genExprMock: unsupported expression " ++ show e

testGenLitArray :: TestTree
testGenLitArray = testGroup "genLitArray"
  [ testCase "Generates array allocation for valid elements" $
      let exprs = [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2, ExprLitInt dummyPos 3]
          (instrs, op, typ) = runGenUnsafe (genLitArray genExprMock exprs)
      in do
        -- Check type
        typ @?= IRPtr (IRArray IRI32 3)
        
        -- Check instructions
        assertBool "Should have IRALLOC_ARRAY" $ any isAllocArray instrs
        
        -- Check operand
        case op of
            IRTemp _ (IRPtr (IRArray IRI32 3)) -> return ()
            _ -> assertFailure $ "Expected IRTemp with correct array type, got: " ++ show op

  , testCase "Throws error for empty array" $
      case runState (runExceptT (genLitArray genExprMock [])) emptyState of
        (Left err, _) -> err @?= "genLitArray: empty array not supported"
        (Right _, _) -> assertFailure "Should have thrown error"
  ]

testGenIndex :: TestTree
testGenIndex = testGroup "genIndex"
  [ testCase "Generates array access" $
      let target = ExprVar dummyPos "arr"
          idx = ExprLitInt dummyPos 0
          (instrs, op, typ) = runGenUnsafe (genIndex genExprMock target idx)
      in do
          typ @?= IRI32
          assertBool "Should have IRGET_ELEM" $ any isGetElem instrs
          case op of
              IRTemp _ IRI32 -> return ()
              _ -> assertFailure "Expected IRTemp result"

  , testCase "Throws error when indexing non-array" $
      let target = ExprVar dummyPos "not_arr"
          idx = ExprLitInt dummyPos 0
      in case runState (runExceptT (genIndex genExprMock target idx)) emptyState of
           (Left err, _) -> assertBool ("Error message should mention array type, got: " ++ err) $ "expected array type" `isInfixOf` err
           (Right _, _) -> assertFailure "Should have thrown error"
  ]

testGenIndexAssign :: TestTree
testGenIndexAssign = testGroup "genIndexAssign"
  [ testCase "Generates array assignment" $
      let target = ExprVar dummyPos "arr"
          idx = ExprLitInt dummyPos 0
          val = ExprLitInt dummyPos 42
          instrs = runGenUnsafe (genIndexAssign genExprMock target idx val)
      in do
          assertBool "Should have IRSET_ELEM" $ any isSetElem instrs
  ]

--
-- helpers
--

isAllocArray :: IRInstruction -> Bool
isAllocArray (IRALLOC_ARRAY _ _ _) = True
isAllocArray _ = False

isGetElem :: IRInstruction -> Bool
isGetElem (IRGET_ELEM _ _ _ _) = True
isGetElem _ = False

isSetElem :: IRInstruction -> Bool
isSetElem (IRSET_ELEM _ _ _) = True
isSetElem _ = False
