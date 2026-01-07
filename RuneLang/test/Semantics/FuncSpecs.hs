module Semantics.FuncSpecs (funcSemanticsTests) where

import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertFailure)

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)

--
-- public
--

funcSemanticsTests :: TestTree
funcSemanticsTests =
  testGroup
    "Rune.Semantics.Func"
    [ testCase "collects functions and overrides" $
        case findFunc mixedProgram of
          Left err -> "HasDuplicates:" `isInfixOf` err @? "Expected HasDuplicates error"
          Right _ -> assertFailure "Expected error for override with same signature",
      testCase "later definitions override earlier entries" $
        let stack = either error id (findFunc shadowProgram)
         in HM.lookup "dup" stack @?= Just [(TypeI32, [TypeI32]), (TypeBool, [TypeBool])],
      testCase "struct method signatures are ignored" $
        findFunc structMethodProgram @?= (Right $ HM.fromList [("show",[(TypeNull,[TypeAny])]),("error",[(TypeNull,[TypeAny])])]),
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
         in HM.lookup "show" stack @?= Just [(TypeNull, [TypeAny]), (TypeNull, [TypeArray TypeAny])]
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
