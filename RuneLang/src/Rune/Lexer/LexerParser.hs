module Rune.Lexer.LexerParser
  ( 
    Parser,
    LexerError,
    TokenConstructor,

   
    makeTokenParser,
    makeReservedTokenParser,
    applyPosition,
  )
where

import Data.Void (Void)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, notFollowedBy, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, string)

type Parser = Parsec Void String
type LexerError = ParseErrorBundle String Void
type TokenConstructor = Int -> Int -> Token

--
-- public
--

-- | as the name suggests
makeTokenParser :: [(TokenKind, String)] -> [Parser TokenConstructor]
makeTokenParser = map makeParser

-- | maps a [] of (token, string) to a [] of reserved token parsers
-- as the name suggests too
makeReservedTokenParser :: [(TokenKind, String)] -> [Parser TokenConstructor]
makeReservedTokenParser = map makeReservedParser

-- | applies position (line, column) to a [] of token constructors
-- abstracting the common pattern of:
-- @
-- choice $ map (\p -> p <*> pure line <*> pure col) parsers
-- @
applyPosition :: [Parser TokenConstructor] -> Int -> Int -> Parser Token
applyPosition parsers line col =
  choice $ map (\p -> p <*> pure line <*> pure col) parsers

--
-- private
--

makeParser :: (TokenKind, String) -> Parser TokenConstructor
makeParser (kind, str) = try $ fmap (Token kind) (string str)

makeReservedParser :: (TokenKind, String) -> Parser TokenConstructor
makeReservedParser (kind, str) = try $ do
  val <- string str
  notFollowedBy (alphaNumChar <|> char '_')
  return $ \line col -> Token kind val line col
