module Semantics.FuncSpecs (funcSemanticsTests) where

import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertFailure)

import Rune.AST.Nodes
import Rune.Semantics.Func

--
-- public
--

funcSemanticsTests :: TestTree
funcSemanticsTests =
  testGroup
    "Rune.Semantics.Func"
    [ findFuncTests
    , findDefsTests
    , transformStructMethodsTests
    , mangleFuncNameTests
    ]

--
-- findFunc tests
--

findFuncTests :: TestTree
findFuncTests = testGroup "findFunc"
  [ testCase "collects functions and overrides" $
      case findFunc mixedProgram of
        Right stack -> do
          HM.member "printer" stack @? "Base function should exist"
          HM.member "null_printer_i32" stack @? "Override function should exist"
        Left err -> assertFailure $ "Expected success, but got error: " ++ err,
    testCase "later definitions override earlier entries" $
      let stack = either error id (findFunc shadowProgram)
       in do
         HM.lookup "dup" stack @?= Just (TypeI32, [Parameter "value" TypeI32 Nothing])
         HM.lookup "bool_dup_bool" stack @?= Just (TypeBool, [Parameter "value" TypeBool Nothing]),
    testCase "struct methods are also collected" $
      findFunc structMethodProgram @?= (Right $ HM.fromList [("Vec_len",(TypeI32,[Parameter "self" (TypeCustom "Vec") Nothing]))]),
    testCase "creates overrides for different signatures" $
      case findFunc duplicateFunctionProgram of
        Right stack -> do
          HM.member "foo" stack @? "Base function should exist"
          HM.member "f32_foo_f32" stack @? "Override should exist"
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "accepts function definition with array of any type (RELOADED)" $
      let stack = either error id (findFunc arrayOverrideProgram)
       in HM.lookup "show" stack @?= Just (TypeNull, [Parameter {paramName = "arr", paramType = TypeArray TypeAny, paramDefault = Nothing}])
  ]

--
-- findDefs tests
--

findDefsTests :: TestTree
findDefsTests = testGroup "findDefs"
  [ testCase "inserts new function definition" $
      case findDefs HM.empty (DefFunction "add" [Parameter "a" TypeI32 Nothing, Parameter "b" TypeI32 Nothing] TypeI32 [] False) of
        Right stack -> do
          case HM.lookup "add" stack of
            Just (TypeI32, params) -> do
              length params @?= 2
              map paramType params @?= [TypeI32, TypeI32]
            _ -> assertFailure "Expected function 'add' in stack"
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects duplicate function with same signature" $
      case findDefs (HM.singleton "add" (TypeI32, [Parameter "a" TypeI32 Nothing, Parameter "b" TypeI32 Nothing])) (DefFunction "add" [Parameter "a" TypeI32 Nothing, Parameter "b" TypeI32 Nothing] TypeI32 [] False) of
        Left err -> "FuncAlreadyExist:" `isInfixOf` err @? "Expected FuncAlreadyExist error"
        Right _ -> assertFailure "Expected error for duplicate signature",
    testCase "appends override to existing function" $
      case findDefs (HM.singleton "show" (TypeNull, [Parameter "value" TypeAny Nothing])) (DefFunction "show" [Parameter "x" TypeI32 Nothing] TypeNull [] False) of
        Right stack -> do
          case HM.lookup "show" stack of
            Just (TypeNull, params) -> do
              length params @?= 1
              case params of
                (p:_) -> paramType p @?= TypeAny
                [] -> assertFailure "Expected at least one parameter"
            _ -> assertFailure "Expected function 'show' in stack"
          case HM.lookup "null_show_i32" stack of
            Just (TypeNull, params) -> do
              length params @?= 1
              case params of
                (p:_) -> paramType p @?= TypeI32
                [] -> assertFailure "Expected at least one parameter"
            _ -> assertFailure "Expected override 'null_show_i32' in stack"
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "processes DefSomewhere with non-override signature" $
      case findDefs HM.empty (DefSomewhere [FunctionSignature "print" [TypeString] TypeNull False]) of
        Right stack -> do
          case HM.lookup "print" stack of
            Just (TypeNull, params) -> do
              length params @?= 1
              case params of
                (p:_) -> paramType p @?= TypeString
                [] -> assertFailure "Expected at least one parameter"
            _ -> assertFailure "Expected function 'print' in stack"
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "processes DefSomewhere with override signature" $
      case findDefs (HM.singleton "print" (TypeNull, [Parameter "value" TypeAny Nothing])) (DefSomewhere [FunctionSignature "print" [TypeString] TypeNull False]) of
        Right stack -> do
          case HM.lookup "print" stack of
            Just (TypeNull, params) -> length params @?= 1
            _ -> assertFailure "Expected function 'print' in stack"
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects struct with duplicate method names" $
      case findDefs HM.empty (DefStruct "Vec" [] [DefFunction "len" [Parameter "self" TypeAny Nothing] TypeI32 [] False, DefFunction "len" [Parameter "self" TypeAny Nothing] TypeI32 [] False]) of
        Left err -> "FuncAlreadyExist:" `isInfixOf` err @? "Expected FuncAlreadyExist error"
        Right _ -> assertFailure "Expected error for duplicate method in struct"
  ]

--
-- transformStructMethods tests
--

transformStructMethodsTests :: TestTree
transformStructMethodsTests = testGroup "transformStructMethods"
  [ testCase "prefixes method names with struct name" $
      let methods = [DefFunction "len" [Parameter "self" TypeAny Nothing] TypeI32 [] False]
          transformed = transformStructMethods "Vec" methods
       in case transformed of
            [DefFunction name _ _ _ _] -> name @?= "Vec_len"
            _ -> assertFailure "Expected single DefFunction in result"
  ]

--
-- mangleFuncName tests
--

mangleFuncNameTests :: TestTree
mangleFuncNameTests = testGroup "mangleFuncName"
  [ testCase "does not mangle when TypeAny is in arguments" $
      mangleFuncName "show" TypeNull [TypeAny] @?= "show",
    testCase "does not mangle when return type is TypeAny" $
      mangleFuncName "foo" TypeAny [TypeI32] @?= "foo",
    testCase "mangles when types are concrete" $
      mangleFuncName "add" TypeI32 [TypeI32, TypeI32] @?= "i32_add_i32_i32"
  ]

--
-- private
--

mixedProgram :: Program
mixedProgram =
  Program
    "mixed"
    [ DefFunction
        "printer"
        [Parameter "text" TypeString Nothing]
        TypeNull
        []
        False,
      DefFunction
        "foo"
        [Parameter "value" TypeI32 Nothing, Parameter "flag" TypeBool Nothing]
        TypeBool
        []
        False,
      DefFunction
        "printer"
        [Parameter "val" TypeI32 Nothing]
        TypeNull
        []
        False,
      DefStruct
        "Vec"
        []
        []
    ]

shadowProgram :: Program
shadowProgram =
  Program
    "shadow"
    [ DefFunction
        "dup"
        [Parameter "value" TypeI32 Nothing]
        TypeI32
        []
        False,
      DefFunction
        "dup"
        [Parameter "value" TypeBool Nothing]
        TypeBool
        []
        False
    ]

structMethodProgram :: Program
structMethodProgram =
  Program
    "struct-methods"
    [ DefStruct
        "Vec"
        []
        [ DefFunction
            "len"
            [Parameter "self" TypeAny Nothing]
            TypeI32
            []
            False
        ]
    ]

duplicateFunctionProgram :: Program
duplicateFunctionProgram =
  Program
    "duplicate-func"
    [ DefFunction "foo" [Parameter "x" TypeI32 Nothing] TypeI32 [] False,
      DefFunction "foo" [Parameter "y" TypeF32 Nothing] TypeF32 [] False
    ]

arrayOverrideProgram :: Program
arrayOverrideProgram =
  Program
    "array-override"
    [ DefFunction "show" [Parameter "arr" (TypeArray TypeAny) Nothing] TypeNull [] False
    ]
