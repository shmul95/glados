module Semantics.FuncSpec (funcSemanticsTests) where

import qualified Data.HashMap.Strict as HM
import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

funcSemanticsTests :: TestTree
funcSemanticsTests =
  testGroup
    "Rune.Semantics.Func"
    [ testCase "collects functions and overrides" $
        let stack = findFunc mixedProgram
         in do
              HM.lookup "foo" stack @?= Just (TypeBool, [TypeI32, TypeBool])
              HM.lookup "printer" stack @?= Just (TypeNull, [TypeString])
              HM.lookup "Vec" stack @?= Nothing,
      testCase "later definitions override earlier entries" $
        let stack = findFunc shadowProgram
         in HM.lookup "dup" stack @?= Just (TypeBool, [TypeBool]),
      testCase "struct method signatures are ignored" $
        findFunc structMethodProgram @?= HM.fromList [("show",(TypeNull,[TypeAny])),("error",(TypeNull,[TypeAny]))]
    ]

mixedProgram :: Program
mixedProgram =
  Program
    "mixed"
    [ DefFunction
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
      DefFunction
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
