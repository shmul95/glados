module Rune.AST.ParserTypes
  ( ParserState (..),
    Parser (..),
  )
where

import Control.Applicative (Alternative (..))
import qualified Rune.Lexer.Tokens as T

--
-- types
--

data ParserState = ParserState
  { psTokens :: [T.Token],
    psPosition :: Int,
    psFilePath :: String
  }
  deriving (Show)

newtype Parser a = Parser {runParser :: ParserState -> Either String (a, ParserState)}

--
-- instances
--

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left err -> Left err
    Right (x, s') -> Right (f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  (Parser pf) <*> (Parser px) = Parser $ \s -> case pf s of
    Left err -> Left err
    Right (f, s') -> case px s' of
      Left err -> Left err
      Right (x, s'') -> Right (f x, s'')

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> case p s of
    Left err -> Left err
    Right (x, s') -> runParser (f x) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Parser empty"
  (Parser p1) <|> (Parser p2) = Parser $ \s ->
    case p1 s of
      Right res -> Right res
      Left _ -> p2 s
