module Semantics.StructSpecs (structSemanticsTests) where

import qualified Data.HashMap.Strict as HM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)

--
-- public
--

structSemanticsTests :: TestTree
structSemanticsTests =
  testGroup
    "Rune.Semantics.Struct"
    [ testCase "struct methods are added with mangled names" $
        case findFunc structMethodProgram of
          Right stack -> HM.lookup "Vec_len" stack @?= Just [(TypeI32, [TypeCustom "Vec"])]
          Left err -> assertFailure $ "Expected success but got error: " <> err,
      testCase "struct method self parameter is corrected to struct type" $
        case findFunc structMethodSelfProgram of
          Right stack -> HM.lookup "Point_distance" stack @?= Just [(TypeF32, [TypeCustom "Point", TypeCustom "Point"])]
          Left err -> assertFailure $ "Expected success but got error: " <> err,
      testCase "multiple struct methods are all registered" $
        case findFunc multiStructMethodProgram of
          Right stack -> do
            HM.lookup "Vec2f_add" stack @?= Just [(TypeCustom "Vec2f", [TypeCustom "Vec2f", TypeCustom "Vec2f"])]
            HM.lookup "Vec2f_scale" stack @?= Just [(TypeCustom "Vec2f", [TypeCustom "Vec2f", TypeF32])]
          Left err -> assertFailure $ "Expected success but got error: " <> err,
      testCase "struct method override adds new signature" $
        case findFunc structMethodOverrideProgram of
          Right stack ->
            HM.lookup "Matrix_multiply" stack @?=
              Just [ (TypeCustom "Matrix", [TypeCustom "Matrix", TypeCustom "Matrix"])
                   , (TypeCustom "Matrix", [TypeCustom "Matrix", TypeF32])
                   ]
          Left err -> assertFailure $ "Expected success but got error: " <> err,
      testCase "struct methods do not interfere with global functions" $
        case findFunc mixedStructAndFuncProgram of
          Right stack -> do
            HM.lookup "Vec_len" stack @?= Just [(TypeI32, [TypeCustom "Vec"])]
            HM.lookup "print" stack @?= Just [(TypeNull, [TypeString])]
          Left err -> assertFailure $ "Expected success but got error: " <> err,
      testCase "empty struct has no methods registered" $
        case findFunc emptyStructProgram of
          Right stack -> HM.lookup "Empty" stack @?= Nothing
          Left err -> assertFailure $ "Expected success but got error: " <> err
    ]

--
-- private
--

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

structMethodSelfProgram :: Program
structMethodSelfProgram =
  Program
    "struct-method-self"
    [ DefStruct
        "Point"
        []
        [ DefFunction
            "distance"
            [Parameter "self" TypeAny, Parameter "other" (TypeCustom "Point")]
            TypeF32
            []
            False
        ]
    ]

multiStructMethodProgram :: Program
multiStructMethodProgram =
  Program
    "multi-struct-methods"
    [ DefStruct
        "Vec2f"
        []
        [ DefFunction
            "add"
            [Parameter "self" TypeAny, Parameter "other" (TypeCustom "Vec2f")]
            (TypeCustom "Vec2f")
            []
            False,
          DefFunction
            "scale"
            [Parameter "self" TypeAny, Parameter "factor" TypeF32]
            (TypeCustom "Vec2f")
            []
            False
        ]
    ]

structMethodOverrideProgram :: Program
structMethodOverrideProgram =
  Program
    "struct-method-override"
    [ DefStruct
        "Matrix"
        []
        [ DefFunction
            "multiply"
            [Parameter "self" TypeAny, Parameter "other" (TypeCustom "Matrix")]
            (TypeCustom "Matrix")
            []
            False,
          DefOverride
            "multiply"
            [Parameter "self" TypeAny, Parameter "scalar" TypeF32]
            (TypeCustom "Matrix")
            []
            False
        ]
    ]

mixedStructAndFuncProgram :: Program
mixedStructAndFuncProgram =
  Program
    "mixed-struct-func"
    [ DefFunction
        "print"
        [Parameter "text" TypeString]
        TypeNull
        []
        False,
      DefStruct
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

emptyStructProgram :: Program
emptyStructProgram =
  Program
    "empty-struct"
    [ DefStruct
        "Empty"
        []
        []
    ]
