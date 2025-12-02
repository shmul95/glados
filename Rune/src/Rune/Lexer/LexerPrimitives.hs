module Rune.Lexer.LexerPrimitives (primitive) where

import Rune.Lexer.LexerParser (Parser, applyPosition, makeReservedTokenParser)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

--
-- public
--

primitive :: Int -> Int -> Parser Token
primitive = applyPosition primitiveParsers

--
-- private
--

primitiveMap :: [(TokenKind, String)]
primitiveMap =
  [ (TypeI8, "i8"),
    (TypeI16, "i16"),
    (TypeI32, "i32"),
    (TypeI64, "i64"),
    (TypeF32, "f32"),
    (TypeF64, "f64"),
    (TypeBool, "bool"),
    (TypeU8, "u8"),
    (TypeU16, "u16"),
    (TypeU32, "u32"),
    (TypeU64, "u64"),
    (TypeString, "string"),
    (TypeAny, "any"),
    (TypeNull, "null")
  ]

primitiveParsers :: [Parser (Int -> Int -> Token)]
primitiveParsers = makeReservedTokenParser primitiveMap
