module AST.Parser.ParseTypesSpecs (parseTypesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Rune.AST.Parser.ParseTypes (parseType, parseIdentifier)
import Rune.AST.Types (Parser(..), ParserState(..))
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseTypesTests :: TestTree
parseTypesTests =
  testGroup
    "AST Parser ParseTypes Tests"
    [ primitiveTypeTests
    , customTypeTests
    , identifierTests
    , arrayTypeTests
    ]

--
-- helpers
--

tok :: T.TokenKind -> T.Token
tok k = T.Token k "" 1 1

runType :: [T.Token] -> Either String Type
runType ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
  in case runParser parseType state of
       Right (t, _) -> Right t
       Left err -> Left err

runIdent :: [T.Token] -> Either String String
runIdent ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
  in case runParser parseIdentifier state of
       Right (s, _) -> Right s
       Left err -> Left err

assertParseType :: String -> T.TokenKind -> Type -> IO ()
assertParseType msg tk expected =
  case runType [tok tk] of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

assertParseCustom :: String -> String -> IO ()
assertParseCustom msg name =
  case runType [tok (T.Identifier name)] of
    Right actual -> assertEqual msg (TypeCustom name) actual
    Left err -> assertBool ("Parse failed: " ++ err) False

assertParseIdent :: String -> String -> IO ()
assertParseIdent msg name =
  case runIdent [tok (T.Identifier name)] of
    Right actual -> assertEqual msg name actual
    Left err -> assertBool ("Parse failed: " ++ err) False

--
-- tests
--

primitiveTypeTests :: TestTree
primitiveTypeTests = testGroup "Primitive Type Tests"
  [ testCase "i8" $ assertParseType "i8" T.TypeI8 TypeI8
  , testCase "i16" $ assertParseType "i16" T.TypeI16 TypeI16
  , testCase "i32" $ assertParseType "i32" T.TypeI32 TypeI32
  , testCase "i64" $ assertParseType "i64" T.TypeI64 TypeI64
  , testCase "u8" $ assertParseType "u8" T.TypeU8 TypeU8
  , testCase "u16" $ assertParseType "u16" T.TypeU16 TypeU16
  , testCase "u32" $ assertParseType "u32" T.TypeU32 TypeU32
  , testCase "u64" $ assertParseType "u64" T.TypeU64 TypeU64
  , testCase "f32" $ assertParseType "f32" T.TypeF32 TypeF32
  , testCase "f64" $ assertParseType "f64" T.TypeF64 TypeF64
  , testCase "bool" $ assertParseType "bool" T.TypeBool TypeBool
  , testCase "char" $ assertParseType "char" T.TypeChar TypeChar
  , testCase "string" $ assertParseType "string" T.TypeString TypeString
  , testCase "any" $ assertParseType "any" T.TypeAny TypeAny
  , testCase "null (type)" $ assertParseType "null" T.TypeNull TypeNull
  , testCase "null (lit)" $ assertParseType "null" T.LitNull TypeNull
  ]

customTypeTests :: TestTree
customTypeTests = testGroup "Custom Type Tests"
  [ testCase "Custom Type" $ assertParseCustom "Vec2" "Vec2"
  ]

identifierTests :: TestTree
identifierTests = testGroup "Identifier Tests"
  [ testCase "Valid Identifier" $ assertParseIdent "x" "x"
  ]

arrayTypeTests :: TestTree
arrayTypeTests = testGroup "Array Type Tests"
  [ testCase "i32[]" $
      case runType [tok T.TypeI32, tok T.LBracket, tok T.RBracket] of
        Right actual -> assertEqual "i32[]" (TypeArray TypeI32) actual
        Left err -> assertBool ("Parse failed: " ++ err) False
  
  , testCase "string[]" $
      case runType [tok T.TypeString, tok T.LBracket, tok T.RBracket] of
        Right actual -> assertEqual "string[]" (TypeArray TypeString) actual
        Left err -> assertBool ("Parse failed: " ++ err) False
  
  , testCase "char[]" $
      case runType [tok T.TypeChar, tok T.LBracket, tok T.RBracket] of
        Right actual -> assertEqual "char[]" (TypeArray TypeChar) actual
        Left err -> assertBool ("Parse failed: " ++ err) False
  
  , testCase "any[]" $
      case runType [tok T.TypeAny, tok T.LBracket, tok T.RBracket] of
        Right actual -> assertEqual "any[]" (TypeArray TypeAny) actual
        Left err -> assertBool ("Parse failed: " ++ err) False
  
  , testCase "Custom[]" $
      case runType [tok (T.Identifier "MyType"), tok T.LBracket, tok T.RBracket] of
        Right actual -> assertEqual "MyType[]" (TypeArray (TypeCustom "MyType")) actual
        Left err -> assertBool ("Parse failed: " ++ err) False
  
  , testCase "bool[]" $
      case runType [tok T.TypeBool, tok T.LBracket, tok T.RBracket] of
        Right actual -> assertEqual "bool[]" (TypeArray TypeBool) actual
        Left err -> assertBool ("Parse failed: " ++ err) False
  ]
