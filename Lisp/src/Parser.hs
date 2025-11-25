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
    <* space

parseLispNumber :: Parser SExpr
parseLispNumber = try $ do
    sign <- optional (char '-')
    digits <- some digitChar
    notFollowedBy (alphaNumChar <|> oneOf "!?_-+*/=<>#")
    let num = read digits :: Int
    return $ Integer $ case sign of
        Just _ -> -num
        Nothing -> num

parseLispString :: Parser SExpr
parseLispString = do
    first <- letterChar <|> oneOf "!?_-+*/=<>#"
    rest <- many (alphaNumChar <|> oneOf "!?_-+*/=<>#")
    let word = first : rest
    return (Symbol word)

parseLispArray :: Parser SExpr
parseLispArray = do
    _ <- char '('
    space
    exprs <- many parseLispValue
    space
    _ <- char ')'
    return (List exprs)