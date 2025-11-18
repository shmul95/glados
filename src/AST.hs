{-
-- EPITECH PROJECT, 2025
-- AST.hs
-- File description:
-- AST.hs
-}

module AST (
    Ast(..),
    sexprToAST,
    handleCall,
    handleString,
    evalAST
) where

import SExpr (SExpr(..))

data Ast = Define Ast Ast
    | Call String [Ast]
    | Lambda [Ast] Ast
    | If Ast Ast Ast
    | AstInteger Int
    | AstSymbol String
    | AstBoolean Bool
    | AstList [Ast]
    deriving (Show, Eq)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (List [Symbol "define", varName, valueExpr]) = do
    astValue <- sexprToAST valueExpr
    astVarName <- sexprToAST varName
    Just (Define astVarName astValue)
sexprToAST (List (Symbol "define" : _)) = Nothing
sexprToAST (List [Symbol "lambda", List args, body]) = do
    astValue <- sexprToAST body
    astArgs <- mapM sexprToAST args
    Just (Lambda astArgs astValue)
sexprToAST (List (Symbol "lambda" : _)) = Nothing
sexprToAST (List [Symbol "if", condExpr, thenExpr, elseExpr]) = do
    astCond <- sexprToAST condExpr
    astThen <- sexprToAST thenExpr
    astElse <- sexprToAST elseExpr
    Just (If astCond astThen astElse)
sexprToAST (List (funcExpr:argExprs)) = do
    astFunc <- sexprToAST funcExpr
    astArgs <- mapM sexprToAST argExprs
    case astFunc of
        AstSymbol op | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] ->
            Just (Call op astArgs)
        _ -> Just (AstList (astFunc:astArgs))
sexprToAST (List exprs) = do
    astExprs <- mapM sexprToAST exprs
    Just (AstList astExprs)

-----------------------------------------------------------------------------------------------
-- Evaluation of AST
-----------------------------------------------------------------------------------------------

type Environment = [(Ast, Ast)]

extractString :: Environment -> Ast -> Maybe String
extractString env (AstSymbol val) = Just val
extractString _ _ = Nothing

compEnv :: Environment -> [(Ast, Ast)] -> String -> Maybe Ast
compEnv _ [] _ = Nothing
compEnv env ((symbol, value):xs) str = do
    strSymbol <- extractString env symbol
    if strSymbol == str then Just value
    else compEnv env xs str

extractInteger :: Environment -> Ast -> Maybe Int
extractInteger env ast = case evalAST env ast of
    Just (AstInteger val) -> Just val
    _                     -> Nothing

handleString :: Environment -> String -> Maybe Ast
handleString _ "#t" = Just (AstBoolean True)
handleString _ "#f" = Just (AstBoolean False)
handleString env s = case compEnv env env s of
    Just value -> evalAST env value
    _          -> Just (AstSymbol s)

handleCall :: Environment -> String -> [Ast] -> Maybe Ast
handleCall env op (x:y:_) | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] = do
    a <- extractInteger env x
    b <- extractInteger env y
    case op of
        "+" -> Just (AstInteger (a + b))
        "-" -> Just (AstInteger (a - b))
        "*" -> Just (AstInteger (a * b))
        "div" -> if b /= 0 then Just (AstInteger (a `div` b)) else Nothing
        "mod" -> if b /= 0 then Just (AstInteger (a `mod` b)) else Nothing
        "eq?" -> Just (AstBoolean (a == b))
        "<" -> Just (AstBoolean (a < b))
        _   -> Nothing
handleCall _ _ _ = Nothing

handleCondition :: Environment -> Ast -> Ast -> Ast -> Maybe Ast
handleCondition env c t e = do
        evaluatedCond <- evalAST env c
        case evaluatedCond of
            AstBoolean True  -> evalAST env t
            AstBoolean False -> evalAST env e
            _                -> Nothing

evalAST :: Environment -> Ast -> Maybe Ast
evalAST env (Define varName value) = do
    evaluatedValue <- evalAST env value
    Just (Define varName evaluatedValue)
evalAST env (Call func args) = handleCall env func args
evalAST env (AstInteger n) = Just (AstInteger n)
evalAST env (AstSymbol s)  = handleString env s
evalAST env (AstBoolean b) = Just (AstBoolean b)
evalAST env (If cond thenExpr elseExpr) = handleCondition env cond thenExpr elseExpr
evalAST env (AstList exprs) = do
    astExprs <- mapM (evalAST env) exprs
    Just (AstList astExprs)
