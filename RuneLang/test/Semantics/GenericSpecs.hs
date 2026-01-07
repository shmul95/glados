module Semantics.GenericSpecs (genericSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Rune.AST.Nodes
import Rune.Semantics.Generic
import Rune.Semantics.Helper (mangleName)

--
-- public
--

genericSemanticsTests :: TestTree
genericSemanticsTests =
  testGroup
    "Rune.Semantics.Generic"
    [ genericInstantiateTests
    , genericReplaceParamTests
    , genericReplaceAnyTests
    ]

--
-- private
--

genericInstantiateTests :: TestTree
genericInstantiateTests = testGroup "instantiate function"
    [ testCase "Instantiate DefFunction with TypeAny params" $
        let 
          def = DefFunction "foo" [Parameter "x" TypeAny] TypeAny [] False
          args = [TypeI32]
          ret = TypeI32
          expectedName = mangleName "foo" ret args
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "x" t] r _ _ -> do
                n @?= expectedName
                t @?= TypeI32
                r @?= TypeI32
            _ -> error "Expected DefFunction"

    , testCase "Instantiate DefOverride with TypeAny params" $
        let 
          def = DefOverride "bar" [Parameter "y" TypeAny] TypeAny [] False
          args = [TypeF32]
          ret = TypeNull
          expectedName = mangleName "bar" ret args
          instantiated = instantiate def args ret
        in case instantiated of
            DefOverride n [Parameter "y" t] r _ _ -> do
                n @?= expectedName
                t @?= TypeF32
                r @?= TypeNull
            _ -> error "Expected DefOverride"

    , testCase "Instantiate preserves non-TypeAny params" $
        let 
          def = DefFunction "baz" [Parameter "a" TypeI32, Parameter "b" TypeAny] TypeAny [] False
          args = [TypeI32, TypeString]
          ret = TypeString
          expectedName = mangleName "baz" ret args
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "a" t1, Parameter "b" t2] r _ _ -> do
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

genericReplaceParamTests :: TestTree
genericReplaceParamTests = testGroup "replaceParam function"
    [ testCase "Replace TypeAny parameter" $
        let 
          param = Parameter "x" TypeAny
          argType = TypeF32
          expected = Parameter "x" TypeF32
          result = replaceParam param argType
        in result @?= expected
    , testCase "Preserve non-TypeAny parameter" $
        let 
          param = Parameter "y" TypeI32
          argType = TypeF32
          expected = Parameter "y" TypeI32
          result = replaceParam param argType
        in result @?= expected
    ]

genericReplaceAnyTests :: TestTree
genericReplaceAnyTests = testGroup "replaceAny function"
    [ testCase "Replace TypeAny with concrete type" $
        let 
          t = TypeAny
          argType = TypeString
          expected = TypeString
          result = replaceAny t argType
        in result @?= expected
    , testCase "Replace TypeArray TypeAny with concrete array type" $
        let 
          t = TypeArray TypeAny
          argType = TypeArray TypeI32
          expected = TypeArray TypeI32
          result = replaceAny t argType
        in result @?= expected
    , testCase "Replace TypeArray TypeAny with non-array type" $
        let 
          t = TypeArray TypeAny
          argType = TypeF32
          expected = TypeArray TypeF32
          result = replaceAny t argType
        in result @?= expected
    , testCase "Preserve non-TypeAny type" $
        let 
          t = TypeI32
          argType = TypeF32
          expected = TypeI32
          result = replaceAny t argType
        in result @?= expected
    ]
