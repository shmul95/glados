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
    SExpr(..)
) where

-- Pandoc Value
data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    deriving Show

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _          = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Integer n) = Just n
getInteger _           = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List xs) = Just xs
getList _         = Nothing

printNode :: [SExpr] -> Maybe String
printNode []     = Just ""
printNode [x]  = printTree x
printNode (x:xs) = do
    strX <- printTree x
    strXs <- printNode xs
    case getList x of
        Just _  -> Just $ "(" ++ strX ++ ")" ++ ", " ++ strXs
        Nothing -> Just $ strX ++ ", " ++ strXs

printList :: [SExpr] -> Maybe String
printList []     = Just ""
printList (x:xs) = do
    strX <- printTree x
    strXs <- printNode xs
    Just $ "a List with " ++ strX ++ " followed by " ++ strXs

printTree :: SExpr -> Maybe String
printTree (Integer n) = Just $ "a Number " ++ show n
printTree (Symbol s)  = Just $ "a Symbol '" ++ s ++ "'"
printTree (List xs)   = printList xs

