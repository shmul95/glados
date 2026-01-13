module Semantics.StructSpecs (structSemanticsTests) where

import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=), assertFailure)

import Rune.AST.Nodes
import Rune.Semantics.Struct

--
-- public
--

structSemanticsTests :: TestTree
structSemanticsTests =
  testGroup
    "Rune.Semantics.Struct"
    [ findStructTests
    , checkFieldsTests
    , checkMethodsTests
    , validateFieldTypeTests
    , helperTests
    ]

helperTests :: TestTree
helperTests = testGroup "Helpers"
  [ testCase "getStructPos returns dummy position" $
      let def = DefStruct "S" [] []
          pos = getStructPos def
      in pos @?= SourcePos "<unknown>" 0 0
  , testCase "mkError formats error message" $
      let msg = mkError (SourcePos "f.ru" 1 1) "expected" "got"
      in "f.ru:1:1" `isInfixOf` msg @? "Should contain position"
  ]

--
-- test groups for each function
--

findStructTests :: TestTree
findStructTests = testGroup "findStruct"
  [ testCase "collects simple structures" $
      case findStruct simpleProgram of
        Right stack -> do
          HM.member "Point" stack @? "Point struct should exist"
          HM.member "Vec2f" stack @? "Vec2f struct should exist"
        Left err -> assertFailure $ "Expected success, got error: " ++ err,
    testCase "allows self-referential struct definitions" $
      case findStruct selfRefProgram of
        Right _ -> return ()
        Left err -> assertFailure $ "Expected success for self-reference, got: " ++ err,
    testCase "rejects duplicate struct definitions" $
      case findStruct duplicateStructProgram of
        Left err -> "duplicate struct definition" `isInfixOf` err @? "Expected duplicate struct error"
        Right _ -> assertFailure "Expected error for duplicate struct"
  ]

checkFieldsTests :: TestTree
checkFieldsTests = testGroup "checkFields"
  [ testCase "rejects duplicate field names" $
      case findStruct duplicateFieldProgram of
        Left err -> "duplicate field" `isInfixOf` err @? "Expected duplicate field error"
        Right _ -> assertFailure "Expected error for duplicate field"
  ]

checkMethodsTests :: TestTree
checkMethodsTests = testGroup "checkMethods"
  [ 
  ]

validateFieldTypeTests :: TestTree
validateFieldTypeTests = testGroup "validateFieldType"
  [ testCase "rejects TypeAny in fields" $
      case findStruct anyFieldProgram of
        Left err -> "type 'any' is not allowed" `isInfixOf` err @? "Expected TypeAny rejection"
        Right _ -> assertFailure "Expected error for TypeAny field",
    testCase "rejects TypeNull in fields" $
      case findStruct nullFieldProgram of
        Left err -> "type 'null' is not allowed" `isInfixOf` err @? "Expected TypeNull rejection"
        Right _ -> assertFailure "Expected error for TypeNull field",
    testCase "rejects unknown struct types" $
      case findStruct unknownTypeProgram of
        Left err -> "unknown struct type" `isInfixOf` err @? "Expected unknown struct error"
        Right _ -> assertFailure "Expected error for unknown struct type"
  ]

--
-- private - test programs
--

simpleProgram :: Program
simpleProgram =
  Program
    "simple"
    [ DefStruct
        "Point"
        [ Field "x" TypeI32
        , Field "y" TypeI32
        ]
        [],
      DefStruct
        "Vec2f"
        [ Field "x" TypeF32
        , Field "y" TypeF32
        ]
        []
    ]

anyFieldProgram :: Program
anyFieldProgram =
  Program
    "any-field"
    [ DefStruct
        "BadStruct"
        [ Field "data" TypeAny
        ]
        []
    ]

nullFieldProgram :: Program
nullFieldProgram =
  Program
    "null-field"
    [ DefStruct
        "BadStruct"
        [ Field "data" TypeNull
        ]
        []
    ]

unknownTypeProgram :: Program
unknownTypeProgram =
  Program
    "unknown-type"
    [ DefStruct
        "Container"
        [ Field "item" (TypeCustom "UnknownStruct")
        ]
        []
    ]

selfRefProgram :: Program
selfRefProgram =
  Program
    "self-ref"
    [ DefStruct
        "Node"
        [ Field "value" TypeI32
        , Field "next" (TypeCustom "Node")
        ]
        []
    ]

duplicateStructProgram :: Program
duplicateStructProgram =
  Program
    "duplicate-struct"
    [ DefStruct "Point" [Field "x" TypeI32] [],
      DefStruct "Point" [Field "y" TypeF32] []
    ]

duplicateFieldProgram :: Program
duplicateFieldProgram =
  Program
    "duplicate-field"
    [ DefStruct
        "BadStruct"
        [ Field "value" TypeI32
        , Field "value" TypeF32
        ]
        []
    ]
