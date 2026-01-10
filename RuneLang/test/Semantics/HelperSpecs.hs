module Semantics.HelperSpecs (helperSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure, assertBool, (@?))
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
  , ("show", [(TypeNull, [TypeAny]), (TypeNull, [TypeArray TypeAny])])
  , ("empty_sigs", [])
  , ("deep_overload", [(TypeString, [TypeString]), (TypeBool, [TypeBool]), (TypeI32, [TypeI32])])
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
    , semanticErrorAccessorsTests
    , typeCompatibleTests
    , specificityTests
    ]

--
-- private tests
--

semanticErrorAccessorsTests :: TestTree
semanticErrorAccessorsTests = testCase "SemanticError accessors" $ do
  let err = SemanticError "test.ru" 10 20 "expected type" "got type" ["context1", "context2"]
  seFile err @?= "test.ru"
  seLine err @?= 10
  seCol err @?= 20
  seExpected err @?= "expected type"
  seGot err @?= "got type"
  seContext err @?= ["context1", "context2"]

typeCompatibleTests :: TestTree
typeCompatibleTests = testGroup "isTypeCompatible additional branches"
  [ testCase "i32 compatible with i64" $ isTypeCompatible TypeI64 TypeI32 @? "i64 < i32"
  , testCase "f32 compatible with f64" $ isTypeCompatible TypeF64 TypeF32 @? "f64 < f32"
  , testCase "i16 compatible with i8"  $ isTypeCompatible TypeI16 TypeI8  @? "i16 < i8"
  , testCase "f64 compatible with f32" $ isTypeCompatible TypeF64 TypeF32 @? "f64 < f32"
  , testCase "int family compatibility" $ isTypeCompatible TypeI64 TypeU32 @? "i64 < u32"
  , testCase "float family compatibility" $ isTypeCompatible TypeF64 TypeF32 @? "f64 < f32"
  , testCase "I32 actual with integer expected (U64)" $ isTypeCompatible TypeU64 TypeI32 @? "u64 < i32"
  , testCase "Incompatible types (I32 and String)" $ isTypeCompatible TypeI32 TypeString @?= False
  ]

specificityTests :: TestTree
specificityTests = testGroup "Signature specificity"
  [ testCase "picks more specific signature (concrete over Any) - branch 1" $ 
      let fs = HM.fromList [("f", [(TypeI64, [TypeI32]), (TypeI32, [TypeAny])])]
      in selectSignature fs "f" [TypeI32] @?= Just TypeI64
  , testCase "picks more specific signature (concrete over Any) - branch 2" $ 
      let fs = HM.fromList [("f", [(TypeI32, [TypeAny]), (TypeI64, [TypeI32])])]
      in selectSignature fs "f" [TypeI32] @?= Just TypeI64
  , testCase "picks more specific signature (concrete array over Any array)" $ 
      let fs = HM.fromList [("f", [(TypeI32, [TypeArray TypeAny]), (TypeI64, [TypeArray TypeI32])])]
      in selectSignature fs "f" [TypeArray TypeI32] @?= Just TypeI64
  , testCase "handles nested arrays specificity" $ 
      let fs = HM.fromList [("f", [(TypeI32, [TypeArray (TypeArray TypeAny)]), (TypeI64, [TypeArray (TypeArray TypeI32)])])]
      in selectSignature fs "f" [TypeArray (TypeArray TypeI32)] @?= Just TypeI64
  ]

mangleNameTests :: TestTree
mangleNameTests = testGroup "mangleName Tests"
  [ testCase "Mangles with primitive types" $ 
      mangleName "add" TypeI32 [TypeI32, TypeI32] @?= "i32_add_i32_i32"
  , testCase "Mangles with custom and mixed types" $ 
      mangleName "print" TypeNull [TypeCustom "Vec", TypeI32] @?= "null_print_Vec_i32"
  , testCase "Mangles with TypeAny in args" $ 
      mangleName "method" TypeNull [TypeAny] @?= "method"
  , testCase "Mangles with TypeAny in return" $ 
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
  , testCase "Existing variable (compatible update)" $ 
      let vs = HM.fromList [("x", TypeI64)]
      in assignVarType vs "x" "test.ru" 1 1 TypeI32 @?= Right (HM.fromList [("x", TypeI32)])
  , testCase "Existing variable (incompatible concrete: i32 -> str) - Error" $ 
      let vs = HM.fromList [("x", TypeI32)]
      in case assignVarType vs "x" "test.ru" 1 1 TypeString of 
          Left _ -> return ()
          Right _ -> assertFailure "Expected error"
  , testCase "Existing variable (incompatible concrete) - Error Content" $ 
      let vs = HM.fromList [("x", TypeI32)]
      in case assignVarType vs "x" "test.ru" 1 1 TypeF32 of 
          Left err -> do 
             seExpected err @?= "variable 'x' to have type i32"
             seGot err @?= "type f32"
             seContext err @?= ["variable assignment", "global context"]
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
  , testCase "TypeArray TypeAny annotation" $ 
      checkMultipleType "v" "test.ru" 1 1 (Just (TypeArray TypeAny)) (TypeArray TypeI32) @?= Right (TypeArray TypeI32)
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
      exprType stack1 (ExprLitInt dummyPos 1) @?= Right TypeI32
      exprType stack1 (ExprLitFloat dummyPos 1.0) @?= Right TypeF32
      exprType stack1 (ExprLitNull dummyPos) @?= Right TypeNull
      exprType stack1 (ExprLitString dummyPos "") @?= Right TypeString
      exprType stack1 (ExprLitChar dummyPos 'a') @?= Right TypeChar
      exprType stack1 (ExprLitBool dummyPos True) @?= Right TypeBool
  , testCase "Array Literal Types" $ do
      exprType stack1 (ExprLitArray dummyPos []) @?= Right (TypeArray TypeAny)
      exprType stack1 (ExprLitArray dummyPos [ExprLitInt dummyPos 1]) @?= Right (TypeArray TypeI32)
      exprType stack1 (ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2, ExprLitInt dummyPos 3]) @?= Right (TypeArray TypeI32)
      exprType stack1 (ExprLitArray dummyPos [ExprLitChar dummyPos 'a', ExprLitChar dummyPos 'b']) @?= Right (TypeArray TypeChar)
      exprType stack1 (ExprLitArray dummyPos [ExprLitString dummyPos "hello", ExprLitString dummyPos "world"]) @?= Right (TypeArray TypeString)
  , testCase "Array Index Types" $ do
      let arr = ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2]
      exprType stack1 (ExprIndex dummyPos arr (ExprLitInt dummyPos 0)) @?= Right TypeI32
      let charArr = ExprLitArray dummyPos [ExprLitChar dummyPos 'R', ExprLitChar dummyPos 'u']
      exprType stack1 (ExprIndex dummyPos charArr (ExprLitInt dummyPos 1)) @?= Right TypeChar
  , testCase "ExprIndex on TypeAny" $ 
      let vs = HM.singleton "a" TypeAny
      in exprType (funcStack1, vs) (ExprIndex dummyPos (ExprVar dummyPos "a") (ExprLitInt dummyPos 0)) @?= Right TypeAny
  , testCase "ExprLitArray incompatible elements - Error" $ 
      (case exprType stack1 (ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitBool dummyPos True]) of 
          Left err -> "IncompatibleArrayElements:" `isInfixOf` err @? "Expected IncompatibleArrayElements error"
          Right _ -> assertFailure "Expected error")
  , testCase "ExprIndex on non-array - Error" $ 
      (case exprType stack1 (ExprIndex dummyPos (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 0)) of 
          Left err -> "IndexingNonArray:" `isInfixOf` err @? "Expected IndexingNonArray error"
          Right _ -> assertFailure "Expected error")
  , testCase "ExprStructInit Type" $ 
      exprType stack1 (ExprStructInit dummyPos "Vec2f" []) @?= Right (TypeCustom "Vec2f")
  , testCase "ExprAccess Type" $ 
      exprType stack1 (ExprAccess dummyPos (ExprVar dummyPos "p") "x") @?= Right TypeAny
  , testCase "ExprVar (exists)" $ 
      exprType stack1 (ExprVar dummyPos "x") @?= Right TypeI32
  , testCase "ExprVar (not exists)" $ 
      exprType stack1 (ExprVar dummyPos "z") @?= Right TypeAny
  , testCase "ExprBinary (valid arithmetic, promotion)" $ 
      exprType stack1 (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)) @?= Right TypeI32
  , testCase "ExprBinary (valid comparison, returns bool)" $ 
      exprType stack1 (ExprBinary dummyPos Eq (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)) @?= Right TypeBool
  , testCase "ExprUnary Type (assumes type does not change)" $ 
      exprType stack1 (ExprUnary dummyPos Negate (ExprVar dummyPos "x")) @?= Right TypeI32
  , testCase "ExprCall (exists and matches single signature)" $ 
      let call = ExprCall dummyPos "foo" [ExprVar dummyPos "x", ExprVar dummyPos "f"]
      in exprType stack1 call @?= Right TypeI32
  , testCase "ExprCall (no matching signature)" $ 
      let call = ExprCall dummyPos "foo" [ExprVar dummyPos "f", ExprVar dummyPos "f"]
      in exprType stack1 call @?= Right TypeAny
  , testCase "ExprCall (unknown function)" $ 
      let call = ExprCall dummyPos "unknownFunc" []
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
  , testCase "Empty list of signatures" $ 
      selectSignature (HM.fromList [("empty", [])]) "empty" [] @?= Nothing
  , testCase "Built-in (Match TypeAny with concrete)" $ 
      selectSignature funcStack1 "any_arg" [TypeI32] @?= Just TypeNull
  , testCase "Array of any (Match with array of i32)" $ 
      selectSignature funcStack1 "show" [TypeArray TypeI32] @?= Just TypeNull
  , testCase "Array of any (Match with array of string)" $ 
      selectSignature funcStack1 "show" [TypeArray TypeString] @?= Just TypeNull
  , testCase "Array of any (Match with array of char)" $ 
      selectSignature funcStack1 "show" [TypeArray TypeChar] @?= Just TypeNull
  , testCase "Unknown Function" $ 
      selectSignature funcStack1 "unknown" [TypeI32] @?= Nothing
  ]

checkEachParamTests :: TestTree
checkEachParamTests = testGroup "checkEachParam Tests"
  [ testCase "Match" $ 
      checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] [TypeI32, TypeF32] @?= Nothing
  , testCase "Mismatch Type - Error Content" $ 
      let result = checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "x"] [TypeI32, TypeF32]
      in case result of 
        Just err -> do 
          seExpected err @?= "argument 1 to have type f32"
          seGot err @?= "type i32"
          seContext err @?= ["parameter check", "function call", "global context"]
        Nothing -> assertFailure "Expected error"
  , testCase "Too Few Arguments - Error Content" $ 
      let result = checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x"] [TypeI32, TypeF32]
      in case result of 
        Just err -> do 
           seExpected err @?= "2 arguments"
           seGot err @?= "1 arguments (too few)"
           seContext err @?= ["parameter count", "function call", "global context"]
        Nothing -> assertFailure "Expected error"
  , testCase "Too Many Arguments - Error Content" $ 
      let result = checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] [TypeI32]
      in case result of 
        Just err -> do 
           seExpected err @?= "1 arguments"
           seGot err @?= "2 arguments (too many)"
           seContext err @?= ["parameter count", "function call", "global context"]
        Nothing -> assertFailure "Expected error"
  , testCase "TypeAny Match" $ 
      checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x"] [TypeAny] @?= Nothing
  , testCase "TypeArray TypeAny (Match with array literal)" $ 
      checkEachParam stack1 "test.ru" 0 0 0 [ExprLitArray dummyPos [ExprLitInt dummyPos 1]] [TypeArray TypeAny] @?= Nothing
  , testCase "TypeArray TypeAny (Mismatch with non-array)" $ 
      let expectedMsg = "argument 0 to have type arrany"
          result = checkEachParam stack1 "test.ru" 0 0 0 [ExprVar dummyPos "x"] [TypeArray TypeAny]
      in assertBool ("Expected msg: " ++ expectedMsg) (case result of Just _ -> True; Nothing -> False)
  , testCase "Nested Expression Error - Error Content" $ 
      let badExpr = ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitString dummyPos "s")
      in (case checkEachParam stack1 "test.ru" 0 0 0 [badExpr] [TypeI32] of 
        Just err -> do 
           seExpected err @?= "valid expression type"
           assertBool "Got should contain WrongType" ("WrongType" `isInfixOf` seGot err)
           seContext err @?= ["parameter check", "function call"]
        Nothing -> assertFailure "Expected error")
  ]

checkParamTypeTests :: TestTree
checkParamTypeTests = testGroup "checkParamType Tests"
  [ testCase "Unknown function - Error Content" $ 
      case checkParamType stack1 "unknown" "test.ru" 0 0 [] of 
          Left err -> do 
             seExpected err @?= "function 'unknown' to exist"
             seGot err @?= "undefined function"
             seContext err @?= ["function call", "global context"]
          Right _ -> assertFailure "Expected error"
  , testCase "Function exists but empty list of signatures - Error Content" $ 
      case checkParamType stack1 "empty_sigs" "f" 1 1 [] of 
          Left err -> do 
             seExpected err @?= "function 'empty_sigs' to exist"
             seGot err @?= "undefined function"
             seContext err @?= ["function call", "global context"]
          Right _ -> assertFailure "Expected error"
  , testCase "Single signature - Match" $ 
      (case checkParamType stack1 "foo" "test.ru" 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] of 
          Right _ -> return ()
          Left _ -> assertFailure "Expected success")
  , testCase "Single signature - Arg Mismatch Error" $ 
      (case checkParamType stack1 "foo" "test.ru" 0 0 [ExprVar dummyPos "f", ExprVar dummyPos "f"] of 
          Left _ -> return ()
          Right _ -> assertFailure "Expected error")
  , testCase "Overloaded - Match i32" $ 
      (case checkParamType stack1 "overloaded" "test.ru" 0 0 [ExprVar dummyPos "x"] of 
          Right name -> name @?= "i32_overloaded_i32"
          Left _ -> assertFailure "Expected success")
  , testCase "Overloaded - Match f32 (mangle)" $ 
      (case checkParamType stack1 "overloaded" "test.ru" 0 0 [ExprVar dummyPos "f"] of 
          Right name -> name @?= "f32_overloaded_f32"
          Left _ -> assertFailure "Expected success")
  , testCase "Overloaded - Match 3rd signature (mangle)" $ 
      -- deep_overload has [String, Bool, I32]
      -- We pass Int, so it skips 1, 2 and matches 3
      (case checkParamType stack1 "deep_overload" "test.ru" 0 0 [ExprLitInt dummyPos 42] of 
          Right name -> name @?= "i32_deep_overload_i32"
          Left _ -> assertFailure "Expected success")
  , testCase "Overloaded - Mismatch all - Error Content" $      case checkParamType stack1 "overloaded" "test.ru" 0 0 [ExprLitBool dummyPos True] of 
          Left err -> do 
             seExpected err @?= "matching signature for overloaded"
             seGot err @?= "no matching overload"
             seContext err @?= ["function call", "global context"]
          Right _ -> assertFailure "Expected error"
  ]

errorFormattingTests :: TestTree
errorFormattingTests = testGroup "Error Formatting Tests"
  [ testCase "formatSemanticError includes file, line, and column" $ 
      (case assignVarType (HM.fromList [("x", TypeI32)]) "x" "example.ru" 42 15 TypeF32 of 
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "example.ru:42:15" errMsg "Should include file:line:col"
              assertInfix "[ERROR]:" errMsg "Should include error prefix"
          Right _ -> assertFailure "Expected error")
  
  , testCase "formatSemanticError includes expected and got sections" $ 
      (case checkMultipleType "myVar" "test.ru" 10 5 (Just TypeI32) TypeF32 of 
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "Expected:" errMsg "Should have Expected section"
              assertInfix "Got:" errMsg "Should have Got section"
          Right _ -> assertFailure "Expected error")
  
  , testCase "formatSemanticError includes context information" $ 
      (case assignVarType (HM.fromList [("y", TypeI64)]) "y" "main.ru" 7 3 TypeBool of 
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "... in" errMsg "Should include context marker"
          Right _ -> assertFailure "Expected error")
  , testCase "formatSemanticError with multiple contexts" $ do
      let err = SemanticError "f.ru" 1 2 "exp" "got" ["ctx1", "ctx2"]
      formatSemanticError err @?= "[ERROR]: f.ru:1:2: error:\n  Expected: exp\n  Got: got\n  ... in ctx1\n  ... in ctx2"
  , testCase "error format for undefined variable" $ 
      (case checkMultipleType "undefinedVar" "app.ru" 20 8 Nothing TypeI32 of 
          Right _ -> return ()  -- Success expected for undefined var with Nothing
          Left _ -> assertFailure "Should succeed for new variable")
  
  , testCase "error format for type mismatch includes both types" $ 
      (case assignVarType (HM.fromList [("z", TypeString)]) "z" "types.ru" 15 12 TypeI32 of 
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "str" errMsg "Should mention string type"
              assertInfix "i32" errMsg "Should mention i32 type"
              assertInfix "types.ru:15:12" errMsg "Should include exact position"
          Right _ -> assertFailure "Expected error")
  
  , testCase "multiple type error includes 'being assigned'" $ 
      (case checkMultipleType "conflictVar" "types.ru" 25 5 (Just TypeF64) TypeI32 of 
          Left err -> do
              let errMsg = formatSemanticError err
              assertInfix "being assigned" errMsg "Should indicate assignment context"
              assertInfix "types.ru:25:5" errMsg "Should include position"
          Right _ -> assertFailure "Expected error")
  ]
  where
    assertInfix :: String -> String -> String -> IO ()
    assertInfix needle haystack msg = 
      if needle `isInfixOf` haystack
        then return ()
        else assertFailure $ msg ++ ": expected to find '" ++ needle ++ "' in '" ++ haystack ++ "'"