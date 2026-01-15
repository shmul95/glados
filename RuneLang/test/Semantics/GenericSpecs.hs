module Semantics.GenericSpecs (genericSemanticsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Rune.AST.Nodes
import Rune.Semantics.Generic

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
          def = DefFunction "foo" [Parameter "x" TypeAny Nothing] TypeAny [] False
          args = [TypeI32]
          ret = TypeI32
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "x" t _] r _ _ -> do
                n @?= "foo"
                t @?= TypeI32
                r @?= TypeI32
            _ -> error "Expected DefFunction"

    , testCase "Instantiate DefFunction with TypeAny params (was override)" $
        let 
          def = DefFunction "bar" [Parameter "y" TypeAny Nothing] TypeAny [] False
          args = [TypeF32]
          ret = TypeNull
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "y" t _] r _ _ -> do
                n @?= "bar"
                t @?= TypeF32
                r @?= TypeNull
            _ -> error "Expected DefFunction"

    , testCase "Instantiate preserves non-TypeAny params" $
        let 
          def = DefFunction "baz" [Parameter "a" TypeI32 Nothing, Parameter "b" TypeAny Nothing] TypeAny [] False
          args = [TypeI32, TypeString]
          ret = TypeString
          instantiated = instantiate def args ret
        in case instantiated of
            DefFunction n [Parameter "a" t1 _, Parameter "b" t2 _] r _ _ -> do
                n @?= "baz"
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
          param = Parameter "x" TypeAny Nothing
          argType = TypeF32
          expected = Parameter "x" TypeF32 Nothing
          result = replaceParam param argType
        in result @?= expected
    , testCase "Preserve non-TypeAny parameter" $
        let 
          param = Parameter "y" TypeI32 Nothing
          argType = TypeF32
          expected = Parameter "y" TypeI32 Nothing
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
