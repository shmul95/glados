module Rune.Lexer.LexerKeywords (keyword) where

import Rune.Lexer.LexerParser (Parser, applyPosition, makeReservedTokenParser)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

--
-- public
--

keyword :: Int -> Int -> Parser Token
keyword = applyPosition keywordParsers

--
-- private
--

keywordMap :: [(TokenKind, String)]
keywordMap =
  [ (KwDef, "def"),
    (KwReturn, "return"),
    (KwStruct, "struct"),
    (KwIf, "if"),
    (KwElse, "else"),
    (KwFor, "for"),
    (KwTo, "to"),
    (KwIn, "in"),
    (KwOverride, "override")
  ]

keywordParsers :: [Parser (Int -> Int -> Token)]
keywordParsers = makeReservedTokenParser keywordMap
