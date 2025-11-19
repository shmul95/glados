{-
-- EPITECH PROJECT, 2025
-- Executer.hs
-- File description:
-- Executer.hs
-}

module Executer (
    executeLisp
) where

import AST (Ast(..), evalAST, sexprToAST)
import SExpr (SExpr)
import Parser (parseLispDocument)
import Type (Parser (..))



executeLisp :: String -> Maybe Ast
executeLisp input = do
    (sexpr, _) <- runParser parseLispDocument input
    ast <- sexprToAST sexpr
    evalAST [] ast
