module Backend.TypesSpec (backendTypesTests) where

import qualified Data.Map.Strict as Map
import Rune.Backend.Types
import Rune.IR.Nodes (IRType (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

backendTypesTests :: TestTree
backendTypesTests =
  testGroup
    "Rune.Backend.Types Specs"
    [ testAsmState,
      testTypeAliases
    ]

testAsmState :: TestTree
testAsmState =
  testGroup
    "AsmState Record"
    [ testCase "AsmState accessors" $
        let stackMap = Map.fromList [("x", -8), ("y", -16)]
            allocMap = Map.fromList [("arr", -24)]
            structs = Map.empty
            externs = ["printf", "malloc"]
            state =
              AsmState
                { asOutput = ["mov rax, 0"],
                  asStackMap = stackMap,
                  asAllocMap = allocMap,
                  asStructs = structs,
                  asExterns = externs
                }
         in do
              asOutput state @?= ["mov rax, 0"]
              asStackMap state @?= stackMap
              asAllocMap state @?= allocMap
              asStructs state @?= structs
              asExterns state @?= externs,
      testCase "Empty AsmState" $
        let emptyState =
              AsmState
                { asOutput = [],
                  asStackMap = Map.empty,
                  asAllocMap = Map.empty,
                  asStructs = Map.empty,
                  asExterns = []
                }
         in do
              length (asOutput emptyState) @?= 0
              Map.null (asStackMap emptyState) @?= True
    ]

testTypeAliases :: TestTree
testTypeAliases =
  testGroup
    "Type Aliases"
    [ testCase "Extern type alias" $
        let ext :: Extern
            ext = "printf"
         in ext @?= "printf",
      testCase "GlobalString type alias" $
        let gs :: GlobalString
            gs = ("str0", "Hello")
         in do
              fst gs @?= "str0"
              snd gs @?= "Hello",
      testCase "Struct type alias" $
        let s :: Struct
            s = [("x", IRI32), ("y", IRI32)]
         in length s @?= 2
    ]
