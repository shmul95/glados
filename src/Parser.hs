{-
-- EPITECH PROJECT, 2025
-- Parser.hs
-- File description:
-- Parser.hs
-}

module Parser (parseLispDocument,
                parseLispArray,
                parseLispNumber,
                parseLispString,
                parseLispValue) where

import Text.Parsec
import Text.Parsec.String (Parser)
import SExpr (SExpr(..))

parseLispDocument :: Parser SExpr
parseLispDocument = do
    spaces
    exprs <- many parseLispValue
    spaces
    eof
    return (List exprs)

parseLispValue :: Parser SExpr
parseLispValue = do
    spaces
    choice [parseLispArray, parseLispNumber, parseLispString]

parseLispNumber :: Parser SExpr
parseLispNumber = do
    spaces
    sign <- optionMaybe (char '-')
    digits <- many1 digit
    notFollowedBy (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['#', '<', '-', '+', '*', '/', '_', '=', '>', '!', '?']))
    spaces
    let num = read digits :: Int
    return $ Integer $ case sign of
        Just _ -> -num
        Nothing -> num

parseLispString :: Parser SExpr
parseLispString = do
    spaces
    word <- many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['#', '<', '-', '+', '*', '/', '_', '=', '>', '!', '?']))
    return (Symbol word)

parseLispArray :: Parser SExpr
parseLispArray = do
    spaces
    _ <- char '('
    spaces
    exprs <- many parseLispValue
    spaces
    _ <- char ')'
    return (List exprs)