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
  [ ("i32_foo_i32_f32", ((TypeI32, [Parameter "a" TypeI32 Nothing, Parameter "b" TypeF32 Nothing]), Public, False))
  , ("i32_overloaded_i32", ((TypeI32, [Parameter "x" TypeI32 Nothing]), Public, False))
  , ("f32_overloaded_f32", ((TypeF32, [Parameter "x" TypeF32 Nothing]), Public, False))
  , ("i32_overloaded_i64", ((TypeI32, [Parameter "x" TypeI64 Nothing]), Public, False))
  , ("any_arg", ((TypeNull, [Parameter "x" TypeAny Nothing]), Public, False))
  , ("show", ((TypeNull, [Parameter "x" TypeAny Nothing]), Public, False))
  , ("null_show_arrany", ((TypeNull, [Parameter "arr" (TypeArray TypeAny) Nothing]), Public, False))
  , ("empty_sigs", ((TypeNull, []), Public, False))
  , ("str_deep_overload_str", ((TypeString, [Parameter "x" TypeString Nothing]), Public, False))
  , ("bool_deep_overload_bool", ((TypeBool, [Parameter "x" TypeBool Nothing]), Public, False))
  , ("i32_deep_overload_i32", ((TypeI32, [Parameter "x" TypeI32 Nothing]), Public, False))
  ]

stack1 :: Stack
stack1 = (funcStack1, HM.fromList [("x", TypeI32), ("f", TypeF32)], HM.empty, HM.empty)

structStack1 :: HM.HashMap String TopLevelDef
structStack1 = HM.fromList
  [ ("Point", DefStruct "Point" 
      [ Field "x" TypeF32 Public False Nothing
      , Field "y" TypeF32 Public False Nothing
      ] [])
  , ("Empty", DefStruct "Empty" [] [])
  , ("Fake", DefFunction "Fake" [] TypeNull [] False Public False)
  ]

paramI32 :: Parameter
paramI32 = Parameter "a" TypeI32 Nothing

paramF32 :: Parameter
paramF32 = Parameter "b" TypeF32 Nothing

paramVariadicI32 :: Parameter
paramVariadicI32 = Parameter "v" (TypeVariadic TypeI32) Nothing

paramWithDefault :: Parameter
paramWithDefault = Parameter "d" TypeI32 (Just (ExprLitInt dummyPos 42))

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
    , getFieldTypeTests
    , fixSelfTypeTests
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
typeCompatibleTests = testGroup "isTypeCompatible"
  [ testCase "Any left" $ isTypeCompatible TypeAny TypeString @?= True
  , testCase "Any right" $ isTypeCompatible TypeString TypeAny @?= True
  , testCase "Ptr Any left" $ isTypeCompatible (TypePtr TypeAny) TypeI32 @?= True
  , testCase "Ptr Any right" $ isTypeCompatible TypeI32 (TypePtr TypeAny) @?= True
  , testCase "Ptr Null match" $ isTypeCompatible (TypePtr TypeI32) TypeNull @?= True
  , testCase "Null Ptr match" $ isTypeCompatible TypeNull (TypePtr TypeF32) @?= True
  , testCase "Ptr Char String" $ isTypeCompatible (TypePtr TypeChar) TypeString @?= True
  , testCase "String Ptr Char" $ isTypeCompatible TypeString (TypePtr TypeChar) @?= True
  , testCase "Recursive Ptr" $ isTypeCompatible (TypePtr TypeI32) (TypePtr TypeI32) @?= True
  , testCase "Ref accepts value" $ isTypeCompatible (TypeRef TypeI32) TypeI32 @?= True
  , testCase "Value passed as Ref" $ isTypeCompatible TypeI32 (TypeRef TypeI32) @?= True
  , testCase "Variadic match" $ isTypeCompatible (TypeVariadic TypeI32) TypeI32 @?= True
  , testCase "Same type match" $ isTypeCompatible TypeBool TypeBool @?= True
  , testCase "I32 promotion" $ isTypeCompatible TypeI64 TypeI32 @?= True
  , testCase "F32 promotion" $ isTypeCompatible TypeF64 TypeF32 @?= True
  , testCase "Int family" $ isTypeCompatible TypeI16 TypeU8 @?= True
  , testCase "Float family" $ isTypeCompatible TypeF64 TypeF32 @?= True
  , testCase "Incompatible mixed" $ isTypeCompatible TypeBool TypeI32 @?= False
  , testCase "Incompatible structs" $ isTypeCompatible (TypeCustom "A") (TypeCustom "B") @?= False
  ]

specificityTests :: TestTree
specificityTests = testGroup "Signature specificity"
  [ testCase "picks more specific signature (concrete over Any)" $ 
      let fs = HM.fromList [("i64_f_i32", ((TypeI64, [Parameter "x" TypeI32 Nothing]), Public, False)), ("i32_f_any", ((TypeI32, [Parameter "x" TypeAny Nothing]), Public, False))]
      in selectSignature fs "f" [TypeI32] @?= Just TypeI64
  , testCase "picks more specific signature (concrete over Any) - branch 2" $ 
      let fs = HM.fromList [("i32_f_any", ((TypeI32, [Parameter "x" TypeAny Nothing]), Public, False)), ("i64_f_i32", ((TypeI64, [Parameter "x" TypeI32 Nothing]), Public, False))]
      in selectSignature fs "f" [TypeI32] @?= Just TypeI64
  , testCase "picks more specific signature (concrete array over Any array)" $ 
      let fs = HM.fromList [("i32_f_arrany", ((TypeI32, [Parameter "x" (TypeArray TypeAny) Nothing]), Public, False)), ("i64_f_arri32", ((TypeI64, [Parameter "x" (TypeArray TypeI32) Nothing]), Public, False))]
      in selectSignature fs "f" [TypeArray TypeI32] @?= Just TypeI64
  , testCase "handles nested arrays specificity" $ 
      let fs = HM.fromList [("i32_f_arrarrany", ((TypeI32, [Parameter "x" (TypeArray (TypeArray TypeAny)) Nothing]), Public, False)), ("i64_f_arrarri32", ((TypeI64, [Parameter "x" (TypeArray (TypeArray TypeI32)) Nothing]), Public, False))]
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
      in assignVarType vs "x" "test.ru" 1 1 TypeAny @?= Right (HM.insert "x" TypeAny vs)
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
      in exprType (funcStack1, vs, HM.empty, HM.empty) (ExprIndex dummyPos (ExprVar dummyPos "a") (ExprLitInt dummyPos 0)) @?= Right TypeAny
  , testCase "ExprLitArray incompatible elements - Error" $ 
      (case exprType stack1 (ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitBool dummyPos True]) of
                Left err -> err @?= "incompatible array element types, expected i32"
                Right _ -> assertFailure "Expected error")  , testCase "ExprIndex on non-array - Error" $ 
      (case exprType stack1 (ExprIndex dummyPos (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 0)) of
                Left err -> err @?= "cannot index type i32"
                Right _ -> assertFailure "Expected error")  , testCase "ExprStructInit Type" $ 
      exprType stack1 (ExprStructInit dummyPos "Vec2f" []) @?= Right (TypeCustom "Vec2f")
  , testCase "ExprAccess Type" $ 
      exprType stack1 (ExprAccess dummyPos (ExprVar dummyPos "p") "x") @?= Left "[ERROR]: test.ru:0:0: error:\n  Expected: field access to be valid on type any\n  Got: cannot access field 'x' on type 'any'\n  ... in field access\n  ... in global context"
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
      let call = ExprCall dummyPos (ExprVar dummyPos "foo") [ExprVar dummyPos "x", ExprVar dummyPos "f"]
      in exprType stack1 call @?= Right TypeI32
  , testCase "ExprCall (no matching signature)" $ 
      let call = ExprCall dummyPos (ExprVar dummyPos "foo") [ExprVar dummyPos "f", ExprVar dummyPos "f"]
      in exprType stack1 call @?= Right TypeAny
  , testCase "ExprCall (unknown function)" $ 
      let call = ExprCall dummyPos (ExprVar dummyPos "unknownFunc") []
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
  , testCase "Function not in stack" $ 
      selectSignature HM.empty "empty" [] @?= Nothing
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
  [ testCase "Match simple: multiple params" $
      checkEachParam stack1 "test.ru" 1 1 0 
        [ExprLitInt dummyPos 1, ExprLitFloat dummyPos 2.0] 
        [paramI32, paramF32] @?= Nothing

  , testCase "Error: Expression type failure (Binary mismatch)" $
      let badExpr = ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitString dummyPos "hi")
          result = checkEachParam stack1 "test.ru" 1 1 0 [badExpr] [paramI32]
      in case result of
           Just err -> do
             seExpected err @?= "valid expression type"
             seContext err @?= ["parameter check", "function call"]
           Nothing -> assertFailure "Should have failed due to invalid expression"

  , testCase "Variadic: Match multiple arguments" $
      checkEachParam stack1 "test.ru" 1 1 0 
        [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2, ExprLitInt dummyPos 3] 
        [paramVariadicI32] @?= Nothing

  , testCase "Variadic: Match zero arguments (empty)" $
      checkEachParam stack1 "test.ru" 1 1 0 [] [paramVariadicI32] @?= Nothing

  , testCase "Variadic: Type mismatch on second variadic arg" $
      let result = checkEachParam stack1 "test.ru" 1 1 0 
                     [ExprLitInt dummyPos 1, ExprLitString dummyPos "fail"] 
                     [paramVariadicI32]
      in case result of
           Just err -> seGot err @?= "type str"
           Nothing -> assertFailure "Should have failed: variadic type mismatch"

  , testCase "Normal: Type mismatch" $
      let result = checkEachParam stack1 "test.ru" 1 1 0 [ExprLitString dummyPos "hi"] [paramI32]
      in case result of
           Just err -> seExpected err @?= "argument 0 to have type i32"
           Nothing -> assertFailure "Should have failed: type mismatch"

  , testCase "Too few arguments: missing required param" $
      let result = checkEachParam stack1 "test.ru" 1 1 0 [] [paramI32]
      in case result of
           Just err -> seGot err @?= "0 arguments (too few)"
           Nothing -> assertFailure "Should have failed: too few arguments"

  , testCase "Default values: missing arg but has default" $
      checkEachParam stack1 "test.ru" 1 1 0 [] [paramWithDefault] @?= Nothing

  , testCase "Too many arguments: extra args are ignored (standard behavior)" $
      checkEachParam stack1 "test.ru" 1 1 0 
        [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2] 
        [paramI32] @?= Nothing

  , testCase "Empty: No args, no params" $
      checkEachParam stack1 "test.ru" 1 1 0 [] [] @?= Nothing
  ]

checkParamTypeTests :: TestTree
checkParamTypeTests = testGroup "checkParamType Tests"
  [ testCase "Unknown function - Error Content" $ 
      case checkParamType stack1 ("unknown", []) "test.ru" 0 0 [] of 
          Left err -> do 
             seExpected err @?= "function 'unknown' to exist"
             seGot err @?= "undefined function"
             seContext err @?= ["function call", "global context"]
          Right _ -> assertFailure "Expected error"
  , testCase "Single signature - Match" $ 
      (case checkParamType stack1 ("foo", [TypeI32, TypeF32]) "test.ru" 0 0 [ExprVar dummyPos "x", ExprVar dummyPos "f"] of 
          Right name -> name @?= "i32_foo_i32_f32"
          Left _ -> assertFailure "Expected success")
  , testCase "Single signature - Arg Mismatch Error" $ 
      (case checkParamType stack1 ("foo", [TypeF32, TypeF32]) "test.ru" 0 0 [ExprVar dummyPos "f", ExprVar dummyPos "f"] of 
          Left _ -> return ()
          Right _ -> assertFailure "Expected error")
  , testCase "Overloaded - Match i32" $ 
      (case checkParamType stack1 ("overloaded", [TypeI32]) "test.ru" 0 0 [ExprVar dummyPos "x"] of 
          Right name -> name @?= "i32_overloaded_i32"
          Left _ -> assertFailure "Expected success")
  , testCase "Overloaded - Match f32 (mangle)" $ 
      (case checkParamType stack1 ("overloaded", [TypeF32]) "test.ru" 0 0 [ExprVar dummyPos "f"] of 
          Right name -> name @?= "f32_overloaded_f32"
          Left _ -> assertFailure "Expected success")
  , testCase "Overloaded - Match 3rd signature (mangle)" $ 
      -- deep_overload has [String, Bool, I32]
      -- We pass Int, so it skips 1, 2 and matches 3
      (case checkParamType stack1 ("deep_overload", [TypeI32]) "test.ru" 0 0 [ExprLitInt dummyPos 42] of 
          Right name -> name @?= "i32_deep_overload_i32"
          Left _ -> assertFailure "Expected success")
  , testCase "Overloaded - Mismatch all - Error Content" $      case checkParamType stack1 ("overloaded", [TypeBool]) "test.ru" 0 0 [ExprLitBool dummyPos True] of 
          Left err -> do 
             seExpected err @?= "function 'overloaded' to exist"
             seGot err @?= "undefined function"
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
          Right _ -> return ()
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

getFieldTypeTests :: TestTree
getFieldTypeTests = testGroup "getFieldType Tests"
  [ testCase "Success: Access existing field" $ 
      getFieldType dummyPos structStack1 (TypeCustom "Point") "x" @?= Right TypeF32

  , testCase "Error: Field does not exist in struct" $ 
      case getFieldType dummyPos structStack1 (TypeCustom "Point") "z" of
        Left err -> do
          seExpected err @?= "field 'z' to exist in struct 'Point'"
          seGot err @?= "undefined field"
        Right _ -> assertFailure "Should have failed: undefined field"

  , testCase "Error: Struct name not found in stack" $ 
      case getFieldType dummyPos structStack1 (TypeCustom "Unknown") "x" of
        Left err -> do
          seExpected err @?= "struct 'Unknown' to exist"
          seGot err @?= "undefined struct"
        Right _ -> assertFailure "Should have failed: undefined struct"

  , testCase "Error: Name exists but is not a struct" $ 
      case getFieldType dummyPos structStack1 (TypeCustom "Fake") "x" of
        Left err -> do
          seExpected err @?= "struct 'Fake' to be a valid struct definition"
          seGot err @?= "not a struct definition"
        Right _ -> assertFailure "Should have failed: not a struct"

  , testCase "Error: Field access on non-custom type (e.g. i32)" $ 
      case getFieldType dummyPos structStack1 TypeI32 "x" of
        Left err -> do
          seExpected err @?= "field access to be valid on type i32"
          seGot err @?= "cannot access field 'x' on type 'i32'"
        Right _ -> assertFailure "Should have failed: invalid type"
  ]

fixSelfTypeTests :: TestTree
fixSelfTypeTests = testGroup "fixSelfType Tests"
  [ testCase "Success: Replaces self type" $ 
      let params = [Parameter "self" TypeAny Nothing, Parameter "x" TypeI32 Nothing]
          result = fixSelfType "Point" params
      in case result of
           (p:_) -> do
             paramName p @?= "self"
             paramType p @?= TypeCustom "Point"
           [] -> assertFailure "Expected non-empty params list"

  , testCase "No change: No parameters" $ 
      fixSelfType "Point" [] @?= []

  , testCase "No change: First parameter is not 'self'" $ 
      let params = [Parameter "x" TypeI32 Nothing, Parameter "self" TypeAny Nothing]
          result = fixSelfType "Point" params
      in case result of
           (p:_) -> paramType p @?= TypeI32
           []    -> assertFailure "Expected non-empty params list"
  ]
