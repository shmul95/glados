module Rune.Lexer.LexerOperators
  ( operator,
  )
where

import Rune.Lexer.LexerParser (Parser, applyPosition, makeTokenParser)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

--
-- public
--

operator :: Int -> Int -> Parser Token
operator = applyPosition operatorParsers

--
-- private
--

operatorMap :: [(TokenKind, String)]
operatorMap =
  [ (OpSquigArrow, "~>"),
    (OpArrow, "->"),
    (OpEq, "=="),
    (OpNeq, "!="),
    (OpLte, "<="),
    (OpGte, ">="),
    (OpAnd, "&&"),
    (OpOr, "||"),
    (OpPlus, "+"),
    (OpMinus, "-"),
    (OpMul, "*"),
    (OpDiv, "/"),
    (OpMod, "%"),
    (OpAssign, "="),
    (OpLt, "<"),
    (OpGt, ">"),
    (OpErrorProp, "?")
  ]

operatorParsers :: [Parser (Int -> Int -> Token)]
operatorParsers = makeTokenParser operatorMap
