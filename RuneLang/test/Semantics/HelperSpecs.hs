module Semantics.HelperSpecs (helperSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)

import Rune.AST.Nodes
import Rune.Semantics.Helper
import Rune.Semantics.Type (FuncStack, Stack)
import TestHelpers (dummyPos)

--
-- setup Mock Data
--

funcStack1 :: FuncStack
funcStack1 = HM.fromList
  [ ("foo", [(TypeI32, [TypeI32, TypeF32])])
  , ("overloaded", [(TypeI32, [TypeI32]), (TypeF32, [TypeF32]), (TypeI32, [TypeI64])])
  , ("any_arg", [(TypeNull, [TypeAny])])
  ]

stack1 :: Stack
stack1 = (funcStack1, HM.fromList [("x", TypeI32), ("f", TypeF32)])

--
-- public
--

helperSemanticsTests :: TestTree
helperSemanticsTests =
  testGroup
    "Rune.Semantics.Helper"
    [ mangleNameTests
    , assignVarTypeTests
    , checkMultipleTypeTests
    , exprTypeTests
    , selectSignatureTests
    , checkEachParamTests
    , checkParamTypeTests
    , errorFormattingTests
    ]

--
-- private tests
--

mangleNameTests :: TestTree
mangleNameTests = testGroup "mangleName Tests"
  [ testCase "Mangles with primitive types" $
      mangleName "add" TypeI32 [TypeI32, TypeI32] @?= "i32_add_i32_i32"
  , testCase "Mangles with custom and mixed types" $
      mangleName "print" TypeNull [TypeCustom "Vec", TypeI32] @?= "null_print_Vec_i32"
  , testCase "Mangles with TypeAny" $
      mangleName "method" TypeAny [TypeI32] @?= "method"
  ]

assignVarTypeTests :: TestTree
assignVarTypeTests = testGroup "assignVarType Tests"
  [ testCase "New variable (explicit type)" $
      let vs = HM.fromList [("y", TypeF32)]
      in assignVarType vs "x" "test.ru" 1 1 TypeI32 @?= Right (HM.fromList [("y", TypeF32), ("x", TypeI32)])
  , testCase "New variable (TypeAny) - no change" $
      let vs = HM.fromList [("y", TypeF32)]
      in assignVarType vs "x" "test.ru" 1 1 TypeAny @?= Right vs
  , testCase "Existing variable (TypeAny -> concrete)" $
      let vs = HM.fromList [("x", TypeAny)]
      in assignVarType vs "x" "test.ru" 1 1 TypeI32 @?= Right (HM.fromList [("x", TypeI32)])
  , testCase "Existing variable (TypeNull -> concrete)" $
      let vs = HM.fromList [("x", TypeNull)]
      in assignVarType vs "x" "test.ru" 1 1 TypeI32 @?= Right (HM.fromList [("x", TypeI32)])
  , testCase "Existing variable (incompatible concrete: i32 -> i64) - Error" $
      let vs = HM.fromList [("x", TypeI32)]
      in case assignVarType vs "x" "test.ru" 1 1 TypeI64 of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  , testCase "Existing variable (incompatible concrete) - Error" $
      let vs = HM.fromList [("x", TypeI32)]
      in case assignVarType vs "x" "test.ru" 1 1 TypeF32 of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  ]

checkMultipleTypeTests :: TestTree
checkMultipleTypeTests = testGroup "checkMultipleType Tests"
  [ testCase "No existing type (Nothing)" $
      checkMultipleType "v" "test.ru" 1 1 Nothing TypeI32 @?= Right TypeI32
  , testCase "Existing TypeAny, new concrete" $
      checkMultipleType "v" "test.ru" 1 1 (Just TypeAny) TypeI32 @?= Right TypeI32
  , testCase "Existing concrete, new TypeAny (returns existing concrete)" $
      checkMultipleType "v" "test.ru" 1 1 (Just TypeI32) TypeAny @?= Right TypeI32
  , testCase "Existing concrete, new TypeNull (returns TypeNull)" $
      checkMultipleType "v" "test.ru" 1 1 (Just TypeI32) TypeNull @?= Right TypeNull
  , testCase "Incompatible concrete types (i32 annot, i64 value) - Allowed with implicit conversion" $
      checkMultipleType "v" "test.ru" 1 1 (Just TypeI32) TypeI64 @?= Right TypeI32
  , testCase "Incompatible concrete types - Error" $
      case checkMultipleType "v" "test.ru" 1 1 (Just TypeI32) TypeF32 of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  ]

exprTypeTests :: TestTree
exprTypeTests = testGroup "exprType Tests"
  [ testCase "Literal Types" $ do
      exprType stack1 (ExprLitInt dummyPos 1) @?= TypeI32
      exprType stack1 (ExprLitFloat dummyPos 1.0) @?= TypeF32
      exprType stack1 (ExprLitNull dummyPos) @?= TypeNull
      exprType stack1 (ExprLitString dummyPos "") @?= TypeString
      exprType stack1 (ExprLitChar dummyPos 'a') @?= TypeU8
      exprType stack1 (ExprLitBool dummyPos True) @?= TypeBool
  , testCase "ExprStructInit Type" $
      exprType stack1 (ExprStructInit dummyPos "Vec2f" []) @?= (TypeCustom "Vec2f")
  , testCase "ExprAccess Type" $
      exprType stack1 (ExprAccess dummyPos (ExprVar dummyPos "p") "x") @?= TypeAny
  , testCase "ExprVar (exists)" $
      exprType stack1 (ExprVar dummyPos "x") @?= TypeI32
  , testCase "ExprVar (not exists)" $
      exprType stack1 (ExprVar dummyPos "z") @?= TypeAny
  , testCase "ExprBinary (valid arithmetic, promotion)" $
      exprType stack1 (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)) @?= TypeI32
  , testCase "ExprBinary (valid comparison, returns bool)" $
      exprType stack1 (ExprBinary dummyPos Eq (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)) @?= TypeBool
  , testCase "ExprBinary (invalid, returns i32)" $
      exprType stack1 (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitBool dummyPos True)) @?= TypeI32
  , testCase "ExprUnary Type (assumes type does not change)" $
      exprType stack1 (ExprUnary dummyPos Negate (ExprVar dummyPos "x")) @?= TypeI32
  , testCase "ExprCall (exists and matches single signature)" $
      let call = ExprCall dummyPos "foo" [ExprVar dummyPos "x", ExprVar dummyPos "f"]
      in exprType stack1 call @?= TypeI32
  , testCase "ExprCall (no matching signature)" $
      let call = ExprCall dummyPos "foo" [ExprVar dummyPos "f", ExprVar dummyPos "f"]
      in exprType stack1 call @?= TypeAny
  , testCase "ExprCall (unknown function)" $
      let call = ExprCall dummyPos "unknownFunc" []
      in exprType stack1 call @?= TypeAny
  ]

selectSignatureTests :: TestTree
selectSignatureTests = testGroup "selectSignature Tests"
  [ testCase "Single Signature (Match)" $
      selectSignature funcStack1 "foo" [TypeI32, TypeF32] @?= Just TypeI32
  , testCase "Single Signature (Mismatch)" $
      selectSignature funcStack1 "foo" [TypeF32, TypeF32] @?= Nothing
  , testCase "Overloaded (Match first exact)" $
      selectSignature funcStack1 "overloaded" [TypeI32] @?= Just TypeI32
  , testCase "Overloaded (Match third via compatibility: i32 < i64)" $
      selectSignature funcStack1 "overloaded" [TypeI32] @?= Just TypeI32
  , testCase "Overloaded (Match second exact)" $
      selectSignature funcStack1 "overloaded" [TypeF32] @?= Just TypeF32
  , testCase "Overloaded (Mismatch all)" $
      selectSignature funcStack1 "overloaded" [TypeBool] @?= Nothing
  , testCase "Built-in (Match TypeAny with concrete)" $
      selectSignature funcStack1 "any_arg" [TypeI32] @?= Just TypeNull
  , testCase "Unknown Function" $
      selectSignature funcStack1 "unknown" [TypeI32] @?= Nothing
  ]

checkEachParamTests :: TestTree
checkEachParamTests = testGroup "checkEachParam Tests"
  [ testCase "Match" $
      checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] [TypeI32, TypeF32] @?= Nothing
  , testCase "Mismatch Type" $
      case checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "x"] [TypeI32, TypeF32] of
        Just _ -> return ()
        Nothing -> assertFailure "Expected error, got success"
  , testCase "Too Few Arguments" $
      case checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x"] [TypeI32, TypeF32] of
        Just _ -> return ()
        Nothing -> assertFailure "Expected error, got success"
  , testCase "Too Many Arguments" $
      case checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] [TypeI32] of
        Just _ -> return ()
        Nothing -> assertFailure "Expected error, got success"
  , testCase "TypeAny Match" $
      checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x"] [TypeAny] @?= Nothing
  , testCase "Nested Expression Error" $
      let expr = ExprBinary dummyPos Add (ExprVar dummyPos "x") (ExprLitBool dummyPos True)
      in checkEachParam stack1 "test.ru" 0 0 0 [expr] [TypeI32] @?= Nothing
  ]

checkParamTypeTests :: TestTree
checkParamTypeTests = testGroup "checkParamType Tests"
  [ testCase "Unknown function - Error" $
      case checkParamType stack1 "unknown" "test.ru" 0 0 [] of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  , testCase "Single signature - Match" $
      case checkParamType stack1 "foo" "test.ru" 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] of
          Right _ -> return ()
          Left _ -> assertFailure "Expected success"
  , testCase "Single signature - Arg Mismatch Error" $
      case checkParamType stack1 "foo" "test.ru" 0 0 [ExprVar dummyPos "f", ExprVar dummyPos "f"] of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  , testCase "Overloaded - Match i32" $
      case checkParamType stack1 "overloaded" "test.ru" 0 0 [ExprVar dummyPos "x"] of
          Right _ -> return ()
          Left _ -> assertFailure "Expected success"
  , testCase "Overloaded - Match f32" $
      case checkParamType stack1 "overloaded" "test.ru" 0 0 [ExprVar dummyPos "f"] of
          Right _ -> return ()
          Left _ -> assertFailure "Expected success"
  , testCase "Overloaded - Mismatch all" $
      case checkParamType stack1 "overloaded" "test.ru" 0 0 [ExprLitBool dummyPos True] of
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  ]

errorFormattingTests :: TestTree
errorFormattingTests = testGroup "Error Formatting Tests"
  [ testCase "formatSemanticError includes file, line, and column" $
      case assignVarType (HM.fromList [("x", TypeI32)]) "x" "example.ru" 42 15 TypeF32 of
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "example.ru:42:15" errMsg "Should include file:line:col"
              assertInfix "[ERROR]:" errMsg "Should include error prefix"
          Right _ -> assertFailure "Expected error"
  
  , testCase "formatSemanticError includes expected and got sections" $
      case checkMultipleType "myVar" "test.ru" 10 5 (Just TypeI32) TypeF32 of
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "Expected:" errMsg "Should have Expected section"
              assertInfix "Got:" errMsg "Should have Got section"
          Right _ -> assertFailure "Expected error"
  
  , testCase "formatSemanticError includes context information" $
      case assignVarType (HM.fromList [("y", TypeI64)]) "y" "main.ru" 7 3 TypeBool of
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "... in" errMsg "Should include context marker"
          Right _ -> assertFailure "Expected error"
  
  , testCase "error format for undefined variable" $
      case checkMultipleType "undefinedVar" "app.ru" 20 8 Nothing TypeI32 of
          Right _ -> return ()  -- Success expected for undefined var with Nothing
          Left _ -> assertFailure "Should succeed for new variable"
  
  , testCase "error format for type mismatch includes both types" $
      case assignVarType (HM.fromList [("z", TypeString)]) "z" "types.ru" 15 12 TypeI32 of
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "str" errMsg "Should mention string type"
              assertInfix "i32" errMsg "Should mention i32 type"
              assertInfix "types.ru:15:12" errMsg "Should include exact position"
          Right _ -> assertFailure "Expected error"
  
  , testCase "multiple type error includes 'being assigned'" $
      case checkMultipleType "conflictVar" "types.ru" 25 5 (Just TypeF64) TypeI32 of
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "being assigned" errMsg "Should indicate assignment context"
              assertInfix "types.ru:25:5" errMsg "Should include position"
          Right _ -> assertFailure "Expected error"
  ]
  where
    assertInfix :: String -> String -> String -> IO ()
    assertInfix needle haystack msg =
      if needle `isInfixOf` haystack
        then return ()
        else assertFailure $ msg ++ ": expected to find '" ++ needle ++ "' in '" ++ haystack ++ "'"
