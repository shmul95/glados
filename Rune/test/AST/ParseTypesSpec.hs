module AST.ParseTypesSpec (parseTypesTests) where

import Data.Either (isRight)
import qualified Rune.AST.Nodes as AST
import Rune.AST.Parser.ParseTypes (parseType)
import Rune.AST.Types (Parser (..), ParserState (..))
import qualified Rune.Lexer.Tokens as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
-- 

parseTypesTests :: TestTree
parseTypesTests =
  testGroup
    "AST.Parser.ParseTypes Specs"
    [ testPrimitiveTypes,
      testCustomTypes,
      testAllTypeTokens
    ]

--
-- private
--

parseTypeHelper :: [T.Token] -> Either String AST.Type
parseTypeHelper tokens =
  let state = ParserState tokens 0 "test.ru" 0
   in case runParser parseType state of
        Left err -> Left err
        Right (result, _) -> Right result

mkToken :: T.TokenKind -> T.Token
mkToken kind = T.Token kind "" 1 1

testPrimitiveTypes :: TestTree
testPrimitiveTypes =
  testGroup
    "primitive type parsing"
    [ testCase "parse i8 type" $ do
        let result = parseTypeHelper [mkToken T.TypeI8]
        result @?= Right AST.TypeI8,
      testCase "parse i16 type" $ do
        let result = parseTypeHelper [mkToken T.TypeI16]
        result @?= Right AST.TypeI16,
      testCase "parse i32 type" $ do
        let result = parseTypeHelper [mkToken T.TypeI32]
        result @?= Right AST.TypeI32,
      testCase "parse i64 type" $ do
        let result = parseTypeHelper [mkToken T.TypeI64]
        result @?= Right AST.TypeI64,
      testCase "parse u8 type" $ do
        let result = parseTypeHelper [mkToken T.TypeU8]
        result @?= Right AST.TypeU8,
      testCase "parse u16 type" $ do
        let result = parseTypeHelper [mkToken T.TypeU16]
        result @?= Right AST.TypeU16,
      testCase "parse u32 type" $ do
        let result = parseTypeHelper [mkToken T.TypeU32]
        result @?= Right AST.TypeU32,
      testCase "parse u64 type" $ do
        let result = parseTypeHelper [mkToken T.TypeU64]
        result @?= Right AST.TypeU64,
      testCase "parse char type" $ do
        let result = parseTypeHelper [mkToken T.TypeChar]
        result @?= Right AST.TypeChar,
      testCase "parse f32 type" $ do
        let result = parseTypeHelper [mkToken T.TypeF32]
        result @?= Right AST.TypeF32,
      testCase "parse f64 type" $ do
        let result = parseTypeHelper [mkToken T.TypeF64]
        result @?= Right AST.TypeF64,
      testCase "parse bool type" $ do
        let result = parseTypeHelper [mkToken T.TypeBool]
        result @?= Right AST.TypeBool,
      testCase "parse string type" $ do
        let result = parseTypeHelper [mkToken T.TypeString]
        result @?= Right AST.TypeString,
      testCase "parse any type" $ do
        let result = parseTypeHelper [mkToken T.TypeAny]
        result @?= Right AST.TypeAny,
      testCase "parse null type" $ do
        let result = parseTypeHelper [mkToken T.TypeNull]
        result @?= Right AST.TypeNull,
      testCase "parse null literal as type" $ do
        let result = parseTypeHelper [mkToken T.LitNull]
        result @?= Right AST.TypeNull
    ]

testCustomTypes :: TestTree
testCustomTypes =
  testGroup
    "custom type parsing"
    [ testCase "parse custom type (struct)" $ do
        let result = parseTypeHelper [T.Token (T.Identifier "Point") "Point" 1 1]
        result @?= Right (AST.TypeCustom "Point"),
      testCase "parse custom type (user-defined)" $ do
        let result = parseTypeHelper [T.Token (T.Identifier "MyType") "MyType" 1 1]
        result @?= Right (AST.TypeCustom "MyType"),
      testCase "parse custom type (single char)" $ do
        let result = parseTypeHelper [T.Token (T.Identifier "T") "T" 1 1]
        result @?= Right (AST.TypeCustom "T")
    ]

testAllTypeTokens :: TestTree
testAllTypeTokens =
  testGroup
    "coverage for all type tokens"
    [ testCase "TypeI8" $ isRight (parseTypeHelper [mkToken T.TypeI8]) @?= True,
      testCase "TypeI16" $ isRight (parseTypeHelper [mkToken T.TypeI16]) @?= True,
      testCase "TypeI32" $ isRight (parseTypeHelper [mkToken T.TypeI32]) @?= True,
      testCase "TypeI64" $ isRight (parseTypeHelper [mkToken T.TypeI64]) @?= True,
      testCase "TypeU8" $ isRight (parseTypeHelper [mkToken T.TypeU8]) @?= True,
      testCase "TypeU16" $ isRight (parseTypeHelper [mkToken T.TypeU16]) @?= True,
      testCase "TypeU32" $ isRight (parseTypeHelper [mkToken T.TypeU32]) @?= True,
      testCase "TypeU64" $ isRight (parseTypeHelper [mkToken T.TypeU64]) @?= True,
      testCase "TypeChar" $ isRight (parseTypeHelper [mkToken T.TypeChar]) @?= True,
      testCase "TypeF32" $ isRight (parseTypeHelper [mkToken T.TypeF32]) @?= True,
      testCase "TypeF64" $ isRight (parseTypeHelper [mkToken T.TypeF64]) @?= True,
      testCase "TypeBool" $ isRight (parseTypeHelper [mkToken T.TypeBool]) @?= True,
      testCase "TypeString" $ isRight (parseTypeHelper [mkToken T.TypeString]) @?= True,
      testCase "TypeAny" $ isRight (parseTypeHelper [mkToken T.TypeAny]) @?= True,
      testCase "TypeNull" $ isRight (parseTypeHelper [mkToken T.TypeNull]) @?= True,
      testCase "LitNull" $ isRight (parseTypeHelper [mkToken T.LitNull]) @?= True
    ]
