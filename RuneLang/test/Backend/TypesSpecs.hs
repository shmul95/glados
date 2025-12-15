{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.TypesSpecs (backendTypesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.Backend.Types
import Rune.IR.Nodes (IRType(..), IRGlobalValue(..))
import qualified Data.Map.Strict as Map

--
-- public
--

backendTypesTests :: TestTree
backendTypesTests = testGroup "Rune.Backend.Types"
  [ testAsmState
  , testTypeAliases
  ]

--
-- private
--

testAsmState :: TestTree
testAsmState = testGroup "AsmState"
  [ testCase "Has asOutput field" $
      let state = AsmState [] Map.empty Map.empty Map.empty []
      in asOutput state @?= []

  , testCase "Has asStackMap field" $
      let state = AsmState [] Map.empty Map.empty Map.empty []
      in Map.size (asStackMap state) @?= 0

  , testCase "Has asAllocMap field" $
      let state = AsmState [] Map.empty Map.empty Map.empty []
      in Map.size (asAllocMap state) @?= 0

  , testCase "Has asStructs field" $
      let state = AsmState [] Map.empty Map.empty Map.empty []
      in Map.size (asStructs state) @?= 0

  , testCase "Has asExterns field" $
      let state = AsmState [] Map.empty Map.empty Map.empty []
      in asExterns state @?= []

  , testCase "Can be constructed with values" $
      let output = ["test"]
          stackMap = Map.singleton "x" (-8)
          allocMap = Map.singleton "ptr" 16
          structs = Map.singleton "Point" [("x", IRI32)]
          externs = ["printf"]
          state = AsmState output stackMap allocMap structs externs
      in do
        asOutput state @?= output
        Map.size (asStackMap state) @?= 1
        Map.size (asAllocMap state) @?= 1
        Map.size (asStructs state) @?= 1
        asExterns state @?= externs
  ]

testTypeAliases :: TestTree
testTypeAliases = testGroup "Type Aliases"
  [ testCase "Extern is String" $
      let ext :: Extern
          ext = "printf"
      in ext @?= "printf"

  , testCase "Global is tuple" $
      let gs :: Global
          gs = ("name", IRGlobalStringVal "value")
      in gs @?= ("name", IRGlobalStringVal "value")

  , testCase "Struct is list of fields" $
      let s :: Struct
          s = [("x", IRI32), ("y", IRF32)]
      in length s @?= 2
  ]
