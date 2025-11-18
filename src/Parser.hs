{-
-- EPITECH PROJECT, 2025
-- Parser.hs
-- File description:
-- Parser.hs
-}

module Parser (parseLispDocument) where

import ParserLib
    ( parseChar
    , parseSpaces
    , parseMany
    , parseSome
    , parseWord
    , parseInt
    )

import Type (Parser (..), (<|>))
import SExpr (SExpr(..))

parseLispDocument :: Parser SExpr
parseLispDocument = List <$> parseMany parseLispValue

parseLispValue :: Parser SExpr
parseLispValue =
        parseLispArray
    <|> parseLispNumber
    <|> parseLispString

parseLispNumber :: Parser SExpr
parseLispNumber = Integer <$> (parseSpaces *> parseInt)

parseLispString :: Parser SExpr
parseLispString = Symbol <$> (parseSpaces *> parseWord)

parseLispArray :: Parser SExpr
parseLispArray = List <$> (parseSpaces *> parseChar '(' *> parseMany parseLispValue <* parseChar ')')