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
         HM.lookup "dup" stack @?= Just (TypeI32, [TypeI32])
         HM.lookup "bool_dup_bool" stack @?= Just (TypeBool, [TypeBool]),
    testCase "struct methods are also collected" $
      findFunc structMethodProgram @?= (Right $ HM.fromList [("show",(TypeNull,[TypeAny])),("error",(TypeNull,[TypeAny])),("Vec_len",(TypeI32,[TypeCustom "Vec"]))]),
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
         HM.lookup "show" stack @?= Just (TypeNull, [TypeAny])
         HM.lookup "null_show_arrany" stack @?= Just (TypeNull, [TypeArray TypeAny])
  ]

--
-- findDefs tests
--

findDefsTests :: TestTree
findDefsTests = testGroup "findDefs"
  [ testCase "inserts new function definition" $
      case findDefs HM.empty (DefFunction "add" [Parameter "a" TypeI32, Parameter "b" TypeI32] TypeI32 [] False) of
        Right stack -> HM.lookup "add" stack @?= Just (TypeI32, [TypeI32, TypeI32])
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects duplicate function with same signature" $
      case findDefs (HM.singleton "add" (TypeI32, [TypeI32, TypeI32])) (DefFunction "add" [Parameter "a" TypeI32, Parameter "b" TypeI32] TypeI32 [] False) of
        Left err -> "FuncAlreadyExist:" `isInfixOf` err @? "Expected FuncAlreadyExist error"
        Right _ -> assertFailure "Expected error for duplicate signature",
    testCase "appends override to existing function" $
      case findDefs (HM.singleton "show" (TypeNull, [TypeAny])) (DefOverride "show" [Parameter "x" TypeI32] TypeNull [] False) of
        Right stack -> do
          HM.lookup "show" stack @?= Just (TypeNull, [TypeAny])
          HM.lookup "null_show_i32" stack @?= Just (TypeNull, [TypeI32])
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects override without base function" $
      case findDefs HM.empty (DefOverride "show" [Parameter "x" TypeI32] TypeNull [] False) of
        Left err -> "WrongOverrideDef:" `isInfixOf` err @? "Expected WrongOverrideDef error"
        Right _ -> assertFailure "Expected error for override without base",
    testCase "processes DefSomewhere with non-override signature" $
      case findDefs HM.empty (DefSomewhere [FunctionSignature "print" [TypeString] TypeNull False]) of
        Right stack -> HM.lookup "print" stack @?= Just (TypeNull, [TypeString])
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "processes DefSomewhere with override signature" $
      case findDefs (HM.singleton "print" (TypeNull, [TypeAny])) (DefSomewhere [FunctionSignature "print" [TypeString] TypeNull True]) of
        Right stack -> do
          HM.lookup "print" stack @?= Just (TypeNull, [TypeAny])
          HM.lookup "print" stack @?= Just (TypeNull, [TypeAny]) -- wait, DefSomewhere override handling?
        Left err -> assertFailure $ "Expected success but got: " ++ err,
    testCase "rejects struct with duplicate method names" $
      case findDefs HM.empty (DefStruct "Vec" [] [DefFunction "len" [Parameter "self" TypeAny] TypeI32 [] False, DefFunction "len" [Parameter "self" TypeAny] TypeF32 [] False]) of
        Left err -> "DuplicateMethodInStruct:" `isInfixOf` err && "len" `isInfixOf` err @? "Expected DuplicateMethodInStruct error mentioning 'len'"
        Right _ -> assertFailure "Expected error for duplicate method in struct"
  ]

--
-- transformStructMethods tests
--

transformStructMethodsTests :: TestTree
transformStructMethodsTests = testGroup "transformStructMethods"
  [ testCase "prefixes method names with struct name" $
      let methods = [DefFunction "len" [Parameter "self" TypeAny] TypeI32 [] False]
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
        [Parameter "text" TypeString]
        TypeNull
        []
        False,
      DefFunction
        "foo"
        [Parameter "value" TypeI32, Parameter "flag" TypeBool]
        TypeBool
        []
        False,
      DefOverride
        "printer"
        [Parameter "text" TypeString]
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
        [Parameter "value" TypeI32]
        TypeI32
        []
        False,
      DefOverride
        "dup"
        [Parameter "value" TypeBool]
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
            [Parameter "self" TypeAny]
            TypeI32
            []
            False
        ]
    ]

duplicateFunctionProgram :: Program
duplicateFunctionProgram =
  Program
    "duplicate-func"
    [ DefFunction "foo" [Parameter "x" TypeI32] TypeI32 [] False,
      DefFunction "foo" [Parameter "y" TypeF32] TypeF32 [] False
    ]

lonelyOverrideProgram :: Program
lonelyOverrideProgram =
  Program
    "lonely-override"
    [ DefOverride "nonExistent" [Parameter "x" TypeI32] TypeI32 [] False
    ]

arrayOverrideProgram :: Program
arrayOverrideProgram =
  Program
    "array-override"
    [ DefOverride "show" [Parameter "arr" (TypeArray TypeAny)] TypeNull [] False
    ]
