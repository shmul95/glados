{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
#define TESTING_EXPORT

module IR.Generator.Expression.StructSpecs (structExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import TestHelpers (dummyPos)
import Rune.IR.Generator.Expression.Struct
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..), GenState(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGenUnsafe, emptyState)

--
-- public
--

structExprTests :: TestTree
structExprTests = testGroup "Rune.IR.Generator.Expression.Struct"
  [ testGenAccess
  , testGenStructInit
  , testResolveStructPtr
  , testLookupFieldType
  , testGenInitField
  ]

--
-- private
--

testGenAccess :: TestTree
testGenAccess = testGroup "genAccess"
  [ testCase "Generates field access for struct" $
      let genExpr (ExprVar _ "p") = return ([], IRTemp "p" (IRStruct "Point"), IRStruct "Point")
          genExpr _             = return ([], IRConstNull, IRNull)
          initialState =
            emptyState
              { gsStructs = Map.fromList [("Point", [("x", IRI32)])]
              }
      in case runState (runExceptT (genAccess genExpr (ExprVar dummyPos "p") "x")) initialState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right (instrs, op, typ), _) -> do
          typ @?= IRI32
          assertBool "Should emit IRGET_FIELD" (any isGetField instrs)
          case op of
            IRTemp _ t -> t @?= IRI32
            _          -> assertFailure "Expected IRTemp result for field access"

  , testCase "Looks up field type" $
      let genExpr (ExprVar _ "p") = return ([], IRTemp "p" (IRPtr (IRStruct "Point")), IRPtr (IRStruct "Point"))
          genExpr _             = return ([], IRConstNull, IRNull)
          initialState =
            emptyState
              { gsStructs = Map.fromList [("Point", [("y", IRI64)])]
              }
      in case runState (runExceptT (genAccess genExpr (ExprVar dummyPos "p") "y")) initialState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right (instrs, _, typ), _) -> do
          typ @?= IRI64
          assertBool "Should not emit IRADDR when operand is already a pointer"
            (not (any isAddr instrs))
  ]

testGenStructInit :: TestTree
testGenStructInit = testGroup "genStructInit"
  [ testCase "Generates struct initialization" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstNull, IRNull)
          (instrs, _, typ) = runGenUnsafe (genStructInit genExpr "Point" [("x", ExprLitInt dummyPos 1)])
      in do
        assertBool "Should have IRALLOC" $ any isAlloc instrs
        case typ of
          IRStruct "Point" -> return ()
          _ -> assertBool "Expected IRStruct Point" False

  , testCase "Generates field initializations" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstNull, IRNull)
          (instrs, _, _) = runGenUnsafe (genStructInit genExpr "Vec" [("x", ExprLitInt dummyPos 1), ("y", ExprLitInt dummyPos 2)])
      in assertBool "Should have instructions" $ not $ null instrs
  ]

testResolveStructPtr :: TestTree
testResolveStructPtr = testGroup "resolveStructPtr"
  [ testCase "Resolves struct temp to pointer" $
      case runState (runExceptT (resolveStructPtr (IRTemp "s" (IRStruct "Point")) (IRStruct "Point"))) emptyState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right (name, op, instrs), _) -> do
          name @?= "Point"
          assertBool "Should have IRADDR" $ not $ null instrs
          case op of
            IRTemp _ (IRPtr _) -> return ()
            _ -> assertBool "Expected IRTemp with IRPtr" False

  , testCase "Handles pointer to struct" $
      case runState (runExceptT (resolveStructPtr (IRTemp "p" (IRPtr (IRStruct "Vec"))) (IRPtr (IRStruct "Vec")))) emptyState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right (name, op, instrs), _) -> do
          name @?= "Vec"
          instrs @?= []
          op @?= IRTemp "p" (IRPtr (IRStruct "Vec"))

  , testCase "Returns Left for non-struct type" $
      case runState (runExceptT (resolveStructPtr (IRTemp "n" IRI32) IRI32)) emptyState of
        (Left _, _)  -> return ()
        (Right _, _) -> assertFailure "Expected Left for resolveStructPtr on non-struct type"
  ]

testLookupFieldType :: TestTree
testLookupFieldType = testGroup "lookupFieldType"
  [ testCase "Returns field type when struct and field exist" $
      let initial =
            emptyState
              { gsStructs = Map.fromList [("Point", [("x", IRI32), ("y", IRI32)])]
              }
      in case runState (runExceptT (lookupFieldType "Point" "x")) initial of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right t, _) -> t @?= IRI32

  , testCase "Returns Left when struct is missing" $
      let initial = emptyState
       in case runState (runExceptT (lookupFieldType "Missing" "x")) initial of
            (Left _, _)  -> return ()
            (Right _, _) -> assertFailure "Expected Left for missing struct in lookupFieldType"

  , testCase "Returns Left when field is missing" $
      let initial =
            emptyState
              { gsStructs = Map.fromList [("Point", [("x", IRI32)])]
              }
       in case runState (runExceptT (lookupFieldType "Point" "y")) initial of
            (Left _, _)  -> return ()
            (Right _, _) -> assertFailure "Expected Left for missing field in lookupFieldType"
  ]

testGenInitField :: TestTree
testGenInitField = testGroup "genInitField"
  [ testCase "Generates address and set-field instructions for struct init" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _              = return ([], IRConstNull, IRNull)
      in case runState (runExceptT (genInitField genExpr "Point" "s0" (IRStruct "Point") ("x", ExprLitInt dummyPos 42))) emptyState of
        (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
        (Right instrs, _) -> case instrs of
          [IRADDR ptrName base (IRPtr sType), IRSET_FIELD (IRTemp ptrOpName (IRPtr sType')) "Point" "x" (IRConstInt 42)] -> do
            base @?= "s0"
            ptrName @?= ptrOpName
            sType @?= IRStruct "Point"
            sType' @?= IRStruct "Point"
          _ -> assertFailure "Expected IRADDR then IRSET_FIELD for genInitField"
  ]

--
-- helpers
--

isAlloc :: IRInstruction -> Bool
isAlloc (IRALLOC _ _) = True
isAlloc _ = False

isGetField :: IRInstruction -> Bool
isGetField (IRGET_FIELD _ _ _ _ _) = True
isGetField _ = False

isAddr :: IRInstruction -> Bool
isAddr (IRADDR _ _ _) = True
isAddr _ = False
