module Semantics.GenericSpecs (genericSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Rune.AST.Nodes
import Rune.Semantics.Generic (instantiate)
import Rune.Semantics.Helper (mangleName)

--
-- public
--

genericSemanticsTests :: TestTree
genericSemanticsTests =
  testGroup
    "Rune.Semantics.Generic"
    [ testCase "Instantiate DefFunction with TypeAny params" $
        let 
          def = DefFunction "foo" [Parameter "x" TypeAny] TypeAny []
          args = [TypeI32]
          ret = TypeI32
          expectedName = mangleName "foo" ret args
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "x" t] r _ -> do
                n @?= expectedName
                t @?= TypeI32
                r @?= TypeI32
            _ -> error "Expected DefFunction"

    , testCase "Instantiate DefOverride with TypeAny params" $
        let 
          def = DefOverride "bar" [Parameter "y" TypeAny] TypeAny []
          args = [TypeF32]
          ret = TypeNull
          expectedName = mangleName "bar" ret args
          instantiated = instantiate def args ret
        in case instantiated of
            DefOverride n [Parameter "y" t] r _ -> do
                n @?= expectedName
                t @?= TypeF32
                r @?= TypeNull
            _ -> error "Expected DefOverride"

    , testCase "Instantiate preserves non-TypeAny params" $
        let 
          def = DefFunction "baz" [Parameter "a" TypeI32, Parameter "b" TypeAny] TypeAny []
          args = [TypeI32, TypeString]
          ret = TypeString
          expectedName = mangleName "baz" ret args
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "a" t1, Parameter "b" t2] r _ -> do
                n @?= expectedName
                t1 @?= TypeI32
                t2 @?= TypeString
                r @?= TypeString
            _ -> error "Expected DefFunction with 2 params"

    , testCase "Instantiate ignores DefStruct" $
        let 
          def = DefStruct "MyStruct" [] []
          instantiated = instantiate def [TypeI32] TypeI32
        in instantiated @?= def
    ]
