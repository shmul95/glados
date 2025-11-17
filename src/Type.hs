{-
-- EPITECH PROJECT, 2025
-- BootstrapPando
-- File description:
-- Type
-}

module Type
  ( Parser (..)
  , Format (..)
  , Config (..)
  , (<|>)
  ) where

import Control.Applicative (Alternative (..))
import Data.List (intercalate)

data Format = XML | JSON | Markdown deriving (Eq)

newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap f p = Parser $ \str ->
    case runParser p str of
      Left e -> Left e
      Right (v, str') -> Right (f v, str')

instance Applicative Parser where
  pure a = Parser $ \str -> Right (a, str)

  (<*>) ab a = Parser $ \str ->
    case runParser ab str of
      Left e -> Left e
      Right (f, str') ->
        case runParser a str' of
          Left e -> Left e
          Right (v, str'') -> Right (f v, str'')

  (<*) a b = Parser $ \str ->
    case runParser a str of
      Left e -> Left e
      Right (v, str') ->
        case runParser b str' of
          Left e -> Left e
          Right (_, str'') -> Right (v, str'')

  (*>) a b = Parser $ \str ->
    case runParser a str of
      Left e -> Left e
      Right (_, str') ->
        case runParser b str' of
          Left e -> Left e
          Right (v', str'') -> Right (v', str'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Empty Parser"

  (<|>) a b = Parser $ \str ->
    case runParser a str of
      Right (v, str') -> Right (v, str')
      Left _ -> runParser b str

instance Monad Parser where
  (>>=) p f = Parser $ \str ->
    case runParser p str of
      Left e -> Left e
      Right (v, str') -> runParser (f v) str'
