module Semantics.HelperSpecs (helperSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertBool, assertFailure)
import Data.List (isInfixOf)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

import Rune.AST.Nodes
import Rune.Semantics.Helper
import Rune.Semantics.Type (FuncStack, Stack)

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
      in assignVarType vs "x" TypeI32 @?= Right (HM.fromList [("y", TypeF32), ("x", TypeI32)])
  , testCase "New variable (TypeAny) - no change" $
      let vs = HM.fromList [("y", TypeF32)]
      in assignVarType vs "x" TypeAny @?= Right vs
  , testCase "Existing variable (TypeAny -> concrete)" $
      let vs = HM.fromList [("x", TypeAny)]
      in assignVarType vs "x" TypeI32 @?= Right (HM.fromList [("x", TypeI32)])
  , testCase "Existing variable (TypeNull -> concrete)" $
      let vs = HM.fromList [("x", TypeNull)]
      in assignVarType vs "x" TypeI32 @?= Right (HM.fromList [("x", TypeI32)])
  , testCase "Existing variable (compatible concrete: i32 -> i64)" $
      let vs = HM.fromList [("x", TypeI32)]
      in assignVarType vs "x" TypeI64 @?= Right (HM.fromList [("x", TypeI64)])
  , testCase "Existing variable (incompatible concrete) - Error" $
      let vs = HM.fromList [("x", TypeI32)]
      in case assignVarType vs "x" TypeF32 of
          Left err -> "TypeOverwrite:" `isInfixOf` err @? "Expected TypeOverwrite error"
          Right _ -> assertFailure "Expected error"
  ]

checkMultipleTypeTests :: TestTree
checkMultipleTypeTests = testGroup "checkMultipleType Tests"
  [ testCase "No existing type (Nothing)" $
      checkMultipleType "v" Nothing TypeI32 @?= Right TypeI32
  , testCase "Existing TypeAny, new concrete" $
      checkMultipleType "v" (Just TypeAny) TypeI32 @?= Right TypeI32
  , testCase "Existing concrete, new TypeAny (returns existing concrete)" $
      checkMultipleType "v" (Just TypeI32) TypeAny @?= Right TypeI32
  , testCase "Existing concrete, new TypeNull (returns TypeNull)" $
      checkMultipleType "v" (Just TypeI32) TypeNull @?= Right TypeNull
  , testCase "Compatible concrete types (i32 annot, i64 value) - returns annotation type" $
      checkMultipleType "v" (Just TypeI32) TypeI64 @?= Right TypeI32
  , testCase "Incompatible concrete types - Error" $
      case checkMultipleType "v" (Just TypeI32) TypeF32 of
          Left err -> "MultipleType:" `isInfixOf` err @? "Expected MultipleType error"
          Right _ -> assertFailure "Expected error"
  ]

exprTypeTests :: TestTree
exprTypeTests = testGroup "exprType Tests"
  [ testCase "Literal Types" $ do
      exprType stack1 (ExprLitInt 1) @?= Right TypeI32
      exprType stack1 (ExprLitFloat 1.0) @?= Right TypeF32
      exprType stack1 ExprLitNull @?= Right TypeNull
      exprType stack1 (ExprLitString "") @?= Right TypeString
      exprType stack1 (ExprLitChar 'a') @?= Right TypeU8
      exprType stack1 (ExprLitBool True) @?= Right TypeBool
  , testCase "ExprStructInit Type" $
      exprType stack1 (ExprStructInit "Vec2f" []) @?= Right (TypeCustom "Vec2f")
  , testCase "ExprAccess Type" $
      exprType stack1 (ExprAccess (ExprVar "p") "x") @?= Right TypeAny
  , testCase "ExprVar (exists)" $
      exprType stack1 (ExprVar "x") @?= Right TypeI32
  , testCase "ExprVar (not exists)" $
      exprType stack1 (ExprVar "z") @?= Right TypeAny
  , testCase "ExprBinary (valid arithmetic, promotion)" $
      exprType stack1 (ExprBinary Add (ExprLitInt 1) (ExprLitInt 2)) @?= Right TypeI64
  , testCase "ExprBinary (valid comparison, returns Bool)" $
      exprType stack1 (ExprBinary Eq (ExprLitInt 1) (ExprLitInt 2)) @?= Right TypeBool
  , testCase "ExprBinary (invalid, propagates iHTBinary error)" $
      case exprType stack1 (ExprBinary Add (ExprLitInt 1) (ExprLitBool True)) of
          Left err -> "WrongType:" `isInfixOf` err @? "Expected WrongType error from iHTBinary"
          Right _ -> assertFailure "Expected error"
  , testCase "ExprUnary Type (assumes type does not change)" $
      exprType stack1 (ExprUnary Negate (ExprVar "x")) @?= Right TypeI32
  , testCase "ExprCall (exists and matches single signature)" $
      let call = ExprCall "foo" [ExprVar "x", ExprVar "f"]
      in exprType stack1 call @?= Right TypeI32
  , testCase "ExprCall (no matching signature)" $
      let call = ExprCall "foo" [ExprVar "f", ExprVar "f"]
      in exprType stack1 call @?= Right TypeAny
  , testCase "ExprCall (unknown function)" $
      let call = ExprCall "unknownFunc" []
      in exprType stack1 call @?= Right TypeAny
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
      checkEachParam stack1 0 [ExprVar "x", ExprVar "f"] [TypeI32, TypeF32] @?= Nothing
  , testCase "Mismatch Type" $
      let expectedMsg = "WrongType: arg1 exp f32 but have i32"
          result = checkEachParam stack1 0 [ExprVar "x", ExprVar "x"] [TypeI32, TypeF32]
      in assertBool ("Expected msg: " ++ expectedMsg) (expectedMsg `isInfixOf` fromMaybe "" result)
  , testCase "Too Few Arguments" $
      let expectedMsg = "WrongNbArgs: exp 2 but 1 were given (too few)"
          result = checkEachParam stack1 0 [ExprVar "x"] [TypeI32, TypeF32]
      in assertBool ("Expected msg: " ++ expectedMsg) (expectedMsg `isInfixOf` fromMaybe "" result)
  , testCase "Too Many Arguments" $
      let expectedMsg = "WrongNbArgs: exp 1 but 2 were given (too many)"
          result = checkEachParam stack1 0 [ExprVar "x", ExprVar "f"] [TypeI32]
      in assertBool ("Expected msg: " ++ expectedMsg) (expectedMsg `isInfixOf` fromMaybe "" result)
  , testCase "TypeAny Match" $
      checkEachParam stack1 0 [ExprVar "x"] [TypeAny] @?= Nothing
  , testCase "Nested Expression Error" $
      let expr = ExprBinary Add (ExprVar "x") (ExprLitBool True)
      in case checkEachParam stack1 0 [expr] [TypeI32] of
        Just err -> "WrongType:" `isInfixOf` err @? "Expected WrongType error from iHTBinary"
        Nothing -> assertFailure "Expected error, got success"
  ]

checkParamTypeTests :: TestTree
checkParamTypeTests = testGroup "checkParamType Tests"
  [ testCase "Unknown function - Error" $
      case checkParamType stack1 "unknown" [] of
          Left err -> "UnknownFunction: unknown is not known" `isInfixOf` err @? "Expected UnknownFunction error"
          Right _ -> assertFailure "Expected error"
  , testCase "Single signature - Match" $
      checkParamType stack1 "foo" [ExprVar "x", ExprVar "f"] @?= Right "foo"
  , testCase "Single signature - Arg Mismatch Error" $
      case checkParamType stack1 "foo" [ExprVar "f", ExprVar "f"] of
          Left err -> "WrongType: arg0 exp i32 but have f32" `isInfixOf` err @? "Expected WrongType error"
          Right _ -> assertFailure "Expected error"
  , testCase "Overloaded - Match i32" $
      checkParamType stack1 "overloaded" [ExprVar "x"] @?= Right "i32_overloaded_i32"
  , testCase "Overloaded - Match f32" $
      checkParamType stack1 "overloaded" [ExprVar "f"] @?= Right "f32_overloaded_f32"
  , testCase "Overloaded - Mismatch all" $
      case checkParamType stack1 "overloaded" [ExprLitBool True] of
          Left err -> "NoMatchingSignature:" `isInfixOf` err @? "Expected NoMatchingSignature error"
          Right _ -> assertFailure "Expected error"
  ]
