module Rune.Lexer.LexerIdentifiers ( identifier ) where

import Rune.Lexer.LexerParser ( Parser,)
import Rune.Lexer.Tokens ( Token (..), TokenKind (..),)
import Text.Megaparsec
  ( many,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
  )

--
-- public
--

identifier :: Int -> Int -> Parser Token
identifier line col = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let val = first : rest
  return $ Token (Identifier val) val line col
