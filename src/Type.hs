{-
-- EPITECH PROJECT, 2025
-- BootstrapPando
-- File description:
-- Type
-}

module Type
  ( Parser (..)
  , (<|>)
  ) where

import Control.Applicative (Alternative (..))
import Data.List (intercalate)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Functor instance
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Just (a, rest) -> Just (f a, rest)
            Nothing        -> Nothing

-- Applicative instance
instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    (Parser pf) <*> (Parser pa) = Parser $ \input -> do
        (f, rest1) <- pf input
        (a, rest2) <- pa rest1
        return (f a, rest2)

-- Alternative instance
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (a, rest) <- p input
        runParser (f a) rest