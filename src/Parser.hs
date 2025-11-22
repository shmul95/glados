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

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import SExpr (SExpr(..))

type Parser = Parsec Void String

parseLispDocument :: Parser SExpr
parseLispDocument = do
    space
    exprs <- many parseLispValue
    space
    eof
    return (List exprs)

parseLispValue :: Parser SExpr
parseLispValue = do
    space
    choice [parseLispArray, parseLispNumber, parseLispString]

parseLispNumber :: Parser SExpr
parseLispNumber = do
    space
    sign <- optional (char '-')
    digits <- some digitChar
    notFollowedBy (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['#', '<', '-', '+', '*', '/', '_', '=', '>', '!', '?']))
    let num = read digits :: Int
    space
    return $ Integer $ case sign of
        Just _ -> -num
        Nothing -> num

parseLispString :: Parser SExpr
parseLispString = do
    space
    word <- some (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['#', '<', '-', '+', '*', '/', '_', '=', '>', '!', '?']))
    return (Symbol word)

parseLispArray :: Parser SExpr
parseLispArray = do
    space
    _ <- char '('
    space
    exprs <- many parseLispValue
    space
    _ <- char ')'
    return (List exprs)