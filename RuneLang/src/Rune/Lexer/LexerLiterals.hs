module Rune.Lexer.LexerLiterals (literal) where

import Control.Monad (void, guard)
import qualified Data.Char as C

import Rune.Lexer.LexerParser (Parser)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
    choice,
    count,
    many,
    noneOf,
    optional,
    some,
    (<|>),
    (<?>)
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
    hexDigitChar,
    octDigitChar,
    string,
  )

--
-- public
--

literal :: Int -> Int -> Parser Token
literal line col =
  choice $ map (\p -> p <*> pure line <*> pure col) literalParsers

--
-- private
--

mkLiteralConstructor :: TokenKind -> String -> (Int -> Int -> Token)
mkLiteralConstructor = Token

intLitParser :: Parser (Int -> Int -> Token)
intLitParser = try $ do
  sign <- optional $ char '-'
  headDigit <- digitChar
  tailDigits <- many (digitChar <|> char '_')

  notFollowedBy $ char '.'
  let rawDigits = headDigit : tailDigits
      cleanDigits = filter (/= '_') rawDigits
      val = maybe cleanDigits (: cleanDigits) sign
      num = read val :: Int
      sourceVal = maybe rawDigits (: rawDigits) sign

  return $ mkLiteralConstructor (LitInt num) sourceVal

floatLitParser :: Parser (Int -> Int -> Token)
floatLitParser = try $ do
  sign <- optional $ char '-'
  whole <- some digitChar
  void $ char '.'
  decimal <- some digitChar
  let val = maybe "" (: []) sign ++ whole ++ "." ++ decimal
      num = read val :: Double

  return $ mkLiteralConstructor (LitFloat num) val

trueLitParser :: Parser (Int -> Int -> Token)
trueLitParser = try $ do
  void $ string "true"
  notFollowedBy (alphaNumChar <|> char '_')
  return $ mkLiteralConstructor (LitBool True) "true"

falseLitParser :: Parser (Int -> Int -> Token)
falseLitParser = try $ do
  void $ string "false"
  notFollowedBy (alphaNumChar <|> char '_')
  return $ mkLiteralConstructor (LitBool False) "false"

boolLitParser :: Parser (Int -> Int -> Token)
boolLitParser = choice [trueLitParser, falseLitParser]

stringLitParser :: Parser (Int -> Int -> Token)
stringLitParser = do
  void $ char '"'
  str <- many stringChar
  void $ char '"'
  let val = "\"" ++ str ++ "\""

  return $ mkLiteralConstructor (LitString str) val

charLitParser :: Parser (Int -> Int -> Token)
charLitParser = try $ do
  void $ char '\''
  c <- charChar
  void $ char '\''

  let val = "'" ++ [c] ++ "'"

  return $ mkLiteralConstructor (LitChar c) val

charChar :: Parser Char
charChar =
  choice
    [ char '\\' >> escapeChar,
      noneOf ['\'', '\\', '\n', '\r', '\t']
    ]

literalParsers :: [Parser (Int -> Int -> Token)]
literalParsers =
  [ floatLitParser,
    intLitParser,
    boolLitParser,
    stringLitParser,
    charLitParser
  ]

stringChar :: Parser Char
stringChar =
  choice
    [ char '\\' >> escapeChar,
      noneOf ['"', '\\']
    ]

escapeChar :: Parser Char
escapeChar =
  choice
    [ char 'n' >> return '\n',
      char 't' >> return '\t',
      char 'r' >> return '\r',
      char '\\' >> return '\\',
      char '"' >> return '"',
      char '\'' >> return '\'',
      try octalEscape,
      try hexEscape,
      try unicodeEscape
    ]

octalEscape :: Parser Char
octalEscape = do
  digits <- choice
    [ try (count 3 octDigitChar)
    , try (count 2 octDigitChar)
    , count 1 octDigitChar
    ]
  let value = foldl (\acc d -> acc * 8 + C.digitToInt d) 0 digits
  guard (value <= 255)
    <?> "Octal escape sequence out of range (max \\377)"
  pure $ C.chr value

hexEscape :: Parser Char
hexEscape = do
  void $ char 'x'
  digits <- choice
    [ try (count 2 hexDigitChar)
    , count 1 hexDigitChar
    ]
  let value = foldl (\acc d -> acc * 16 + C.digitToInt d) 0 digits
  guard (value <= 255)
    <?> "Hexadecimal escape sequence out of range (max \\xFF)"
  pure $ C.chr value

unicodeEscape :: Parser Char
unicodeEscape = do
  void $ char 'u'
  value <- choice
    [ bracedUnicode
    , unbracedUnicode
    ]
  guard (value <= 0x10FFFF)
    <?> "Unicode escape sequence out of range (max \\u{10FFFF})"
  pure $ C.chr value
  where

  bracedUnicode :: Parser Int
  bracedUnicode = do
    void $ char '{'
    digits <- some hexDigitChar
    void $ char '}'
    return $ foldl (\acc d -> acc * 16 + C.digitToInt d) 0 digits

  unbracedUnicode :: Parser Int
  unbracedUnicode = do
    digits <- choice
      [ try (count 4 hexDigitChar)
      , try (count 6 hexDigitChar)
      ]
    pure $ foldl (\acc d -> acc * 16 + C.digitToInt d) 0 digits
