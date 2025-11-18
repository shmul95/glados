{-
-- EPITECH PROJECT, 2025
-- Pandoc
-- File description:
-- ParserLib
-}

module ParserLib
  ( parseChar,
    parseOr,
    parseAnyChar,
    parseAndWith,
    parseAnd,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseDouble,
    parseTuple,
    parseTruple,
    parseWord,
    parseIgnoreSpaces,
    parseSeparator,
    sepBy,
    parseString,
    parseAnyCharNot,
    parseSpaces
  )
where

import Control.Applicative (many)
import Data.Functor (($>))
import Type (Parser (..), (<|>))

parseChar :: Char -> Parser Char
parseChar c = Parser parse
  where
    parse [] = Nothing
    parse (s : str)
      | c == s = Just (c, str)
      | otherwise = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b = a <|> b

parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) (Parser $ const Nothing)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f a b = f <$> a <*> b

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)

parseMany :: Parser a -> Parser [a]
parseMany a = (:) <$> a <*> parseMany a <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome a = (:) <$> a <*> parseMany a

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0' .. '9'])

parseInt :: Parser Int
parseInt = (negate <$> (parseChar '-' *> parseUInt)) <|> parseUInt

parseDouble :: Parser Double
parseDouble = do
  a <- parseInt
  _ <- parseChar '.'
  b <- parseUInt
  case a > 0 of
    True -> return $ fromIntegral a + (fromIntegral b / 10 ^ length (show b))
    False -> return $ fromIntegral a - (fromIntegral b / 10 ^ length (show b))

parseTuple :: Parser a -> Parser (a, a)
parseTuple p =
  (,)
    <$> (parseChar '(' *> p)
    <*> (parseSeparator (parseChar ',') *> p <* parseChar ')')

parseTruple :: Parser (Int, Int, Int)
parseTruple = do
  _ <- parseChar '('
  a <- parseInt
  _ <- parseSeparator $ parseChar ','
  b <- parseInt
  _ <- parseSeparator $ parseChar ','
  c <- parseInt
  _ <- parseChar ')'
  return (a, b, c)

parseSpaces :: Parser String
parseSpaces = parseMany (parseAnyChar " \n\t")

parseWord :: Parser String
parseWord = parseSome . parseAnyChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['-', '+', '*', '/', '_', '=','<','>','!','?']

parseIgnoreSpaces :: Parser ()
parseIgnoreSpaces = (parseMany . parseAnyChar $ " \n\t") $> ()

parseSeparator :: Parser Char -> Parser Char
parseSeparator p = parseIgnoreSpaces *> p <* parseIgnoreSpaces

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

parseString :: String -> Parser String
parseString = mapM parseChar

parseAnyCharNot :: String -> Parser Char
parseAnyCharNot forbidden = Parser parse
  where
    parse [] = Nothing
    parse (x : xs)
      | x `elem` forbidden = Nothing
      | otherwise = Just (x, xs)
