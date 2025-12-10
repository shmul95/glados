module Semantics.FuncSpec (funcSemanticsTests) where

import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertFailure)

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)

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
        findFunc structMethodProgram @?= (Right $ HM.fromList [("show",[(TypeNull,[TypeAny])]),("error",[(TypeNull,[TypeAny])])])
    ]

mixedProgram :: Program
mixedProgram =
  Program
    "mixed"
    [ DefFunction
        "printer"
        [Parameter "text" TypeString]
        TypeNull
        [],
      DefFunction
        "foo"
        [Parameter "value" TypeI32, Parameter "flag" TypeBool]
        TypeBool
        [],
      DefOverride
        "printer"
        [Parameter "text" TypeString]
        TypeNull
        [],
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
        [],
      DefOverride
        "dup"
        [Parameter "value" TypeBool]
        TypeBool
        []
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
        ]
    ]
