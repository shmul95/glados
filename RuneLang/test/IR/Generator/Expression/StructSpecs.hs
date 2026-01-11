{-# LANGUAGE OverloadedStrings #-}

module IR.Generator.Expression.StructSpecs (structExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Rune.IR.Generator.Expression.Struct
import Rune.IR.Nodes
import Rune.AST.Nodes
import IR.TestUtils (emptyState)
import TestHelpers (dummyPos)

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

structExprTests :: TestTree
structExprTests = testGroup "Rune.IR.Generator.Expression.Struct"
  [ testGenAccess
  , testGenStructInit
  , testResolveStructPtr
  , testLookupFieldType
  , testGenInitField
  ]

mockGenExpr :: GenExprCallback
mockGenExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
mockGenExpr (ExprVar _ name) = return ([], IRTemp name (IRStruct "Point"), IRStruct "Point") -- Simplified
mockGenExpr _ = return ([], IRConstInt 0, IRI32)

testGenAccess :: TestTree
testGenAccess = testGroup "genAccess"
  [ testCase "Generates field access for struct" $
      let state = emptyState 
            { gsStructs = Map.singleton "Point" [("x", IRI32), ("y", IRI32)] }
          res = evalState (runExceptT (genAccess mockGenExpr (ExprVar dummyPos "p") "x")) state
      in case res of
           Right (_, _, typ) -> typ @?= IRI32
           Left err -> assertBool ("Access failed: " ++ err) False

  , testCase "Looks up field type" $
      let state = emptyState 
            { gsStructs = Map.singleton "Point" [("x", IRI32)] }
          res = evalState (runExceptT (lookupFieldType "Point" "x")) state
      in case res of
           Right typ -> typ @?= IRI32
           Left err -> assertBool ("Lookup failed: " ++ err) False
  ]

testGenStructInit :: TestTree
testGenStructInit = testGroup "genStructInit"
  [ testCase "Generates struct initialization" $
      let state = emptyState 
            { gsStructs = Map.singleton "Point" [("x", IRI32), ("y", IRI32)] }
          res = evalState (runExceptT (genStructInit mockGenExpr "Point" [])) state
      in case res of
           Right (instrs, _, typ) -> do
             typ @?= IRStruct "Point"
             -- Check for alloc and default init instructions
             assertBool "Should have alloc and init" (length instrs >= 1)
           Left err -> assertBool ("Struct init failed: " ++ err) False

  , testCase "Generates field initializations" $
      let state = emptyState 
            { gsStructs = Map.singleton "Vec" [("val", IRI32)] }
          res = evalState (runExceptT (genStructInit mockGenExpr "Vec" [("val", ExprLitInt dummyPos 42)])) state
      in case res of
           Right (instrs, _, _) -> do
             assertBool "Should contain set field instr" $ 
               any (\i -> case i of IRSET_FIELD _ "Vec" "val" _ -> True; _ -> False) instrs
           Left err -> assertBool ("Field init failed: " ++ err) False
  ]

testResolveStructPtr :: TestTree
testResolveStructPtr = testGroup "resolveStructPtr"
  [ testCase "Resolves struct temp to pointer" $
      let res = evalState (runExceptT (resolveStructPtr (IRTemp "t" (IRStruct "S")) (IRStruct "S"))) emptyState
      in case res of
           Right ("S", _, instrs) -> length instrs @?= 1 -- Should add IRADDR
           Right _ -> assertBool "Expected struct S" False
           Left err -> assertBool ("Resolve failed: " ++ err) False

  , testCase "Handles pointer to struct" $
      let res = evalState (runExceptT (resolveStructPtr (IRTemp "p" (IRPtr (IRStruct "S"))) (IRPtr (IRStruct "S")))) emptyState
      in case res of
           Right ("S", _, instrs) -> instrs @?= [] -- No new instrs needed
           Right _ -> assertBool "Expected struct S" False
           Left err -> assertBool ("Resolve ptr failed: " ++ err) False

  , testCase "Returns Left for non-struct type" $
      let res = evalState (runExceptT (resolveStructPtr (IRConstInt 1) IRI32)) emptyState
      in case res of
           Left _ -> return ()
           Right _ -> assertBool "Should have failed" False
  ]

testLookupFieldType :: TestTree
testLookupFieldType = testGroup "lookupFieldType"
  [ testCase "Returns field type when struct and field exist" $
      let state = emptyState { gsStructs = Map.singleton "S" [("f", IRI32)] }
          res = evalState (runExceptT (lookupFieldType "S" "f")) state
      in case res of
           Right t -> t @?= IRI32
           Left err -> assertBool ("Lookup failed: " ++ err) False

  , testCase "Returns Left when struct is missing" $
      let res = evalState (runExceptT (lookupFieldType "Unknown" "f")) emptyState
      in case res of
           Left _ -> return ()
           Right _ -> assertBool "Should have failed" False

  , testCase "Returns Left when field is missing" $
      let state = emptyState { gsStructs = Map.singleton "S" [("f", IRI32)] }
          res = evalState (runExceptT (lookupFieldType "S" "g")) state
      in case res of
           Left _ -> return ()
           Right _ -> assertBool "Should have failed" False
  ]

testGenInitField :: TestTree
testGenInitField = testGroup "genInitField"
  [ testCase "Generates address and set-field instructions for struct init" $
      let res = evalState (runExceptT (genInitField mockGenExpr "S" "base" (IRStruct "S") ("f", ExprLitInt dummyPos 42))) emptyState
      in case res of
           Right instrs -> length instrs @?= 2 -- ADDR + SET_FIELD
           Left err -> assertBool ("Gen init field failed: " ++ err) False
  ]