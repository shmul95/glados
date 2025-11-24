{-
-- EPITECH PROJECT, 2025
-- Executor.hs
-- File description:
-- Executor.hs
-}

module Executor (
    executeLisp
) where

import AST (Ast(..), evalAST, sexprToAST)
import Parser (parseLispDocument)
import Text.Megaparsec (parse, errorBundlePretty)

executeLisp :: String -> Either String Ast
executeLisp input = case parse parseLispDocument "" input of
        Left err -> Left $ "Parse error: " ++ errorBundlePretty err
        Right sexpr -> do
            case sexprToAST sexpr of
                Nothing -> Left "AST conversion error"
                Just ast -> case evalAST [] ast of
                    Nothing -> Left "Evaluation error"
                    Just result -> Right result

-- AstInteger n -> show n
-- AstBoolean True -> "#t"
-- AstBoolean False -> "#f"
-- AstSymbol s -> s
-- AstLambda _ _ _ -> "#\<procedure\>"
-- _ -> ""
