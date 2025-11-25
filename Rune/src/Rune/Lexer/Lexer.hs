{-# LANGUAGE OverloadedStrings #-}

module Rune.Lexer.Lexer
  ( lexer,
    LexerError,
  )
where

import Rune.Lexer.LexerDelimiters (delimiter)
import Rune.Lexer.LexerIdentifiers (identifier)
import Rune.Lexer.LexerKeywords (keyword)
import Rune.Lexer.LexerLiterals (literal)
import Rune.Lexer.LexerOperators (operator)
import Rune.Lexer.LexerParser
  ( LexerError,
    Parser,
  )
import Rune.Lexer.LexerPrimitives (primitive)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Text.Megaparsec
  ( MonadParsec (eof),
    choice,
    getSourcePos,
    many,
    runParser,
    sourceColumn,
    sourceLine,
    unPos,
  )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
  ( skipBlockComment,
    skipLineComment,
    space,
  )

--
-- public
--

lexer :: String -> String -> Either LexerError [Token]
lexer = runParser tokens

--
-- private
--

tokens :: Parser [Token]
tokens = do
  spaceConsumer
  ts <- many token
  pos <- getSourcePos
  let line = unPos $ sourceLine pos
      col = unPos $ sourceColumn pos
      eofToken = Token EOF "" line col
  eof
  return $ ts ++ [eofToken]

token :: Parser Token
token = do
  pos <- getSourcePos
  let line = unPos $ sourceLine pos
      col = unPos $ sourceColumn pos
  choice (getToken line col) <* spaceConsumer

getToken :: Int -> Int -> [Parser Token]
getToken line col =
  [ keyword line col,
    primitive line col,
    literal line col,
    operator line col,
    delimiter line col,
    identifier line col
  ]

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")
