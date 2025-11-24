module Rune.Lexer.LexerLiterals (literal) where

import Control.Monad (void)
import Rune.Lexer.LexerParser (Parser)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
    choice,
    many,
    noneOf,
    optional,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
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
  digits <- some digitChar

  notFollowedBy $ char '.'
  let val = maybe digits (: digits) sign
      num = read val :: Int

  return $ mkLiteralConstructor (LitInt num) val

floatLitParser :: Parser (Int -> Int -> Token)
floatLitParser = try $ do
  sign <- optional $ char '-'
  whole <- some digitChar
  void $ char '.'
  decimal <- some digitChar
  let val = maybe "" (: []) sign ++ whole ++ "." ++ decimal
      num = read val :: Double

  return $ mkLiteralConstructor (LitFloat num) val

boolLitParser :: Parser (Int -> Int -> Token)
boolLitParser =
  choice
    [ mkBoolParser True "true",
      mkBoolParser False "false"
    ]
  where
    mkBoolParser b str = try $ do
      void $ string str
      notFollowedBy (alphaNumChar <|> char '_')

      return $ mkLiteralConstructor (LitBool b) str

stringLitParser :: Parser (Int -> Int -> Token)
stringLitParser = do
  void $ char '"'
  str <- many stringChar
  void $ char '"'
  let val = "\"" ++ str ++ "\""

  return $ mkLiteralConstructor (LitString str) val

literalParsers :: [Parser (Int -> Int -> Token)]
literalParsers =
  [ floatLitParser,
    intLitParser,
    boolLitParser,
    stringLitParser
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
      char '"' >> return '"'
    ]
