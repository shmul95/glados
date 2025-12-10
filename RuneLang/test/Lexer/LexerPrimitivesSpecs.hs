module Lexer.LexerPrimitivesSpecs (lexerPrimitivesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Rune.Lexer.Tokens (TokenKind (..))
import Lexer.Utils (tok, lexTest)

--
-- public
--

lexerPrimitivesTests :: TestTree
lexerPrimitivesTests =
  testGroup
    "LexerPrimitives Tests"
    [ test_prim_signed_int
    , test_prim_unsigned_int
    , test_prim_float
    , test_prim_bool_char_string
    , test_prim_any_null
    , test_prim_reserved_check
    ]

--
-- private
--

test_prim_signed_int :: TestTree
test_prim_signed_int = testCase "Signed Integer Types (i8, i16, i32, i64)" $
  lexTest "i8 i16 i32 i64"
    [ tok TypeI8 "i8" 1 1, tok TypeI16 "i16" 1 4, tok TypeI32 "i32" 1 8, tok TypeI64 "i64" 1 12, tok EOF "" 1 15 ]

test_prim_unsigned_int :: TestTree
test_prim_unsigned_int = testCase "Unsigned Integer Types (u8, u16, u32, u64)" $
  lexTest "u8 u16 u32 u64"
    [ tok TypeU8 "u8" 1 1, tok TypeU16 "u16" 1 4, tok TypeU32 "u32" 1 8, tok TypeU64 "u64" 1 12, tok EOF "" 1 15 ]

test_prim_float :: TestTree
test_prim_float = testCase "Float Types (f32, f64)" $
  lexTest "f32 f64"
    [ tok TypeF32 "f32" 1 1, tok TypeF64 "f64" 1 5, tok EOF "" 1 8 ]

test_prim_bool_char_string :: TestTree
test_prim_bool_char_string = testCase "Bool, Char, String Types" $
  lexTest "bool char string"
    [ tok TypeBool "bool" 1 1, tok TypeChar "char" 1 6, tok TypeString "string" 1 11, tok EOF "" 1 17 ]

test_prim_any_null :: TestTree
test_prim_any_null = testCase "Any, Null Types" $
  lexTest "any null"
    [ tok TypeAny "any" 1 1, tok TypeNull "null" 1 5, tok EOF "" 1 9 ]

test_prim_reserved_check :: TestTree
test_prim_reserved_check = testCase "Primitive reserved check (i32_var should be ID)" $
  lexTest "i32_var f64X"
    [ tok (Identifier "i32_var") "i32_var" 1 1, tok (Identifier "f64X") "f64X" 1 9, tok EOF "" 1 13 ]
