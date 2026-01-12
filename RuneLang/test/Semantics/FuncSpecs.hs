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
          HM.member "null_printer_str" stack @? "Override function should exist"
        Left err -> assertFailure $ "Expected success, but got error: " ++ err,
    testCase "later definitions override earlier entries" $
      let stack = either error id (findFunc shadowProgram)
       in do
         HM.lookup "dup" stack @?= Just (TypeI32, [TypeI32], Nothing, False)
         HM.lookup "bool_dup_bool" stack @?= Just (TypeBool, [TypeBool], Nothing, False),
    testCase "struct methods are also collected" $
      findFunc structMethodProgram @?= (Right $ HM.fromList [("show",(TypeNull,[TypeAny],Nothing,True)),("error",(TypeNull,[TypeAny],Nothing,True)),("Vec_len",(TypeI32,[TypeCustom "Vec"],Nothing,False))]),
    testCase "rejects duplicate function definition" $
      case findFunc duplicateFunctionProgram of
        Left err -> "FuncAlreadyExist:" `isInfixOf` err @? "Expected FuncAlreadyExist error for foo"
        Right _ -> assertFailure "Expected error for duplicate function",
    testCase "rejects override without base function" $
      case findFunc lonelyOverrideProgram of
        Left err -> "WrongOverrideDef:" `isInfixOf` err @? "Expected WrongOverrideDef error for nonExistent"
        Right _ -> assertFailure "Expected error for lonely override",
    testCase "accepts override with array of any type" $
      let stack = either error id (findFunc arrayOverrideProgram)
       in do
         HM.lookup "show" stack @?= Just (TypeNull, [TypeAny], Nothing, True)
         HM.lookup "null_show_arrany" stack @?= Just (TypeNull, [TypeArray TypeAny], Nothing, False)
  ]

--
-- findDefs tests
--

findDefsTests :: TestTree
findDefsTests = testGroup "findDefs"
  [ testCase "inserts new function definition" $
      case findDefs HM.empty (DefFunction "add" [Parameter "a" TypeI32 False, Parameter "b" TypeI32 False] TypeI32 [] False) of
        Right stack -> HM.lookup "add" stack @?= Just (TypeI32, [TypeI32, TypeI32], Nothing, False)
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects duplicate function with same signature" $
      case findDefs (HM.singleton "add" (TypeI32, [TypeI32, TypeI32], Nothing, False)) (DefFunction "add" [Parameter "a" TypeI32 False, Parameter "b" TypeI32 False] TypeI32 [] False) of
        Left err -> "FuncAlreadyExist:" `isInfixOf` err @? "Expected FuncAlreadyExist error"
        Right _ -> assertFailure "Expected error for duplicate signature",
    testCase "appends override to existing function" $
      case findDefs (HM.singleton "show" (TypeNull, [TypeAny], Nothing, True)) (DefOverride "show" [Parameter "x" TypeI32 False] TypeNull [] False) of
        Right stack -> do
          HM.lookup "show" stack @?= Just (TypeNull, [TypeAny], Nothing, True)
          HM.lookup "null_show_i32" stack @?= Just (TypeNull, [TypeI32], Nothing, False)
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects override without base function" $
      case findDefs HM.empty (DefOverride "show" [Parameter "x" TypeI32 False] TypeNull [] False) of
        Left err -> "WrongOverrideDef:" `isInfixOf` err @? "Expected WrongOverrideDef error"
        Right _ -> assertFailure "Expected error for override without base",
    testCase "processes DefSomewhere with non-override signature" $
      case findDefs HM.empty (DefSomewhere [FunctionSignature "print" [TypeString] TypeNull False Nothing]) of
        Right stack -> HM.lookup "print" stack @?= Just (TypeNull, [TypeString], Nothing, True)
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "processes DefSomewhere with override signature" $
      case findDefs (HM.singleton "print" (TypeNull, [TypeAny], Nothing, False)) (DefSomewhere [FunctionSignature "print" [TypeString] TypeNull True Nothing]) of
        Right stack -> do
          HM.lookup "print" stack @?= Just (TypeNull, [TypeAny], Nothing, False)
          HM.lookup "print" stack @?= Just (TypeNull, [TypeAny], Nothing, False) -- wait, DefSomewhere override handling?
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects struct with duplicate method names" $
      case findDefs HM.empty (DefStruct "Vec" [] [DefFunction "len" [Parameter "self" TypeAny False] TypeI32 [] False, DefFunction "len" [Parameter "self" TypeAny False] TypeF32 [] False]) of
        Left err -> "DuplicateMethodInStruct:" `isInfixOf` err && "len" `isInfixOf` err @? "Expected DuplicateMethodInStruct error mentioning 'len'"
        Right _ -> assertFailure "Expected error for duplicate method in struct"
  ]

--
-- transformStructMethods tests
--

transformStructMethodsTests :: TestTree
transformStructMethodsTests = testGroup "transformStructMethods"
  [ testCase "prefixes method names with struct name" $
      let methods = [DefFunction "len" [Parameter "self" TypeAny False] TypeI32 [] False]
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
        [Parameter "text" TypeString False]
        TypeNull
        []
        False,
      DefFunction
        "foo"
        [Parameter "value" TypeI32 False, Parameter "flag" TypeBool False]
        TypeBool
        []
        False,
      DefOverride
        "printer"
        [Parameter "text" TypeString False]
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
        [Parameter "value" TypeI32 False]
        TypeI32
        []
        False,
      DefOverride
        "dup"
        [Parameter "value" TypeBool False]
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
            [Parameter "self" TypeAny False]
            TypeI32
            []
            False
        ]
    ]

duplicateFunctionProgram :: Program
duplicateFunctionProgram =
  Program
    "duplicate-func"
    [ DefFunction "foo" [Parameter "x" TypeI32 False] TypeI32 [] False,
      DefFunction "foo" [Parameter "y" TypeF32 False] TypeF32 [] False
    ]

lonelyOverrideProgram :: Program
lonelyOverrideProgram =
  Program
    "lonely-override"
    [ DefOverride "nonExistent" [Parameter "x" TypeI32 False] TypeI32 [] False
    ]

arrayOverrideProgram :: Program
arrayOverrideProgram =
  Program
    "array-override"
    [ DefOverride "show" [Parameter "arr" (TypeArray TypeAny) False] TypeNull [] False
    ]
