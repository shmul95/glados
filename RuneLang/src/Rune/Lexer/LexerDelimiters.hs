module Rune.Lexer.LexerDelimiters (delimiter) where

import Rune.Lexer.LexerParser (Parser, applyPosition, makeTokenParser)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

--
-- public
--

delimiter :: Int -> Int -> Parser Token
delimiter = applyPosition delimiterParsers

--
-- private
--

delimiterMap :: [(TokenKind, String)]
delimiterMap =
  [ (LParen, "("),
    (RParen, ")"),
    (LBrace, "{"),
    (RBrace, "}"),
    (Comma, ","),
    (Semicolon, ";"),
    (Colon, ":"),
    (Dot, "."),
    (LBracket, "["),
    (RBracket, "]")
  ]

delimiterParsers :: [Parser (Int -> Int -> Token)]
delimiterParsers = makeTokenParser delimiterMap
