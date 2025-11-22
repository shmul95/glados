{-
-- EPITECH PROJECT, 2025
-- Parser.hs
-- File description:
-- Parser.hs
-}

-- Requirement in the file :

-- - Contains a common interface for parsers:
-- - each parser (XML, JSON, Markdown) must implement a function
-- - `parseDocument :: String -> Either String Document`
-- - This module defines the `Parser` type and some potential helpers.

module SExpr (
    SExpr(..),
    getSymbol,
    getInteger,
    getList,
    printTree,
    describeList,
    describeListRest
) where

-- Pandoc Value
data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    deriving (Show, Eq)

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _          = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Integer n) = Just n
getInteger _           = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List xs) = Just xs
getList _         = Nothing

printTree :: SExpr -> Maybe String
printTree (Symbol s) = Just ("a Symbol " ++ s)
printTree (Integer n) = Just ("a Number " ++ show n)
printTree (List xs) = do
    xs' <- describeList xs
    return $ "a List with " ++ xs'

describeList :: [SExpr] -> Maybe String
describeList [] = Just "nothing in it"
describeList (x:xs) = do
    x' <- printTree x
    xs' <- describeListRest xs
    return $ x' ++ " followed by " ++ xs'

describeListRest :: [SExpr] -> Maybe String
describeListRest [] = Just ""
describeListRest [x] = printTree x
describeListRest (x:xs) = do
    x' <- printTree x
    xs' <- describeListRest xs
    return $ x' ++ ", " ++ xs'

