{-
-- EPITECH PROJECT, 2025
-- AST.hs
-- File description:
-- AST.hs
-}

module AST (
    Ast(..),
    sexprToAST,
    handleArg,
    handleCall,
    handleString,
    evalAST
) where

import SExpr (SExpr(..))

data Ast = Define String Ast
    | Call String [Ast]
    | AstInteger Int
    | AstFloat Float
    | AstSymbol String
    | AstBoolean Bool
    | AstList [Ast]
    deriving Show

handleString :: String -> Ast
handleString "#t" = AstBoolean True
handleString "#f" = AstBoolean False
handleString s = AstSymbol s

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (List [Symbol "define", Symbol varName, valueExpr]) = do
    astValue <- sexprToAST valueExpr
    Just (Define varName astValue)
sexprToAST (List (Symbol "define" : _)) = Nothing
sexprToAST (List (Symbol "+": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "+" astArgs)
sexprToAST (List (Symbol "-": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "-" astArgs)
sexprToAST (List (Symbol "*": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "*" astArgs)
sexprToAST (List (Symbol "div": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "div" astArgs)
sexprToAST (List (Symbol "eq?": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "eq?" astArgs)
sexprToAST (List (Symbol "mod": args)) = do
    astArgs <- mapM sexprToAST args
    Just (Call "mod" astArgs)
sexprToAST (List exprs) = do
    astExprs <- mapM sexprToAST exprs
    Just (AstList astExprs)


handleArg :: Ast -> Maybe Ast
handleArg (AstInteger n) = Just (AstInteger n)
handleArg (AstFloat f)   = Just (AstFloat f)
handleArg (Call func args) = handleCall func args
handleArg _ = Nothing


handleCall :: String -> [Ast] -> Maybe Ast
handleCall op (x:y:_) | op `elem` ["+", "-", "*", "div", "mod", "eq?"] = do
    a <- extractInteger x
    b <- extractInteger y
    case op of
        "+" -> Just (AstInteger (a + b))
        "-" -> Just (AstInteger (a - b))
        "*" -> Just (AstInteger (a * b))
        "div" -> if b /= 0 then Just (AstInteger (a `div` b)) else Nothing
        "mod" -> if b /= 0 then Just (AstInteger (a `mod` b)) else Nothing
        "eq?" -> Just (AstBoolean (a == b))
        _   -> Nothing
handleCall _ _ = Nothing

extractInteger :: Ast -> Maybe Int
extractInteger ast = case handleArg ast of
    Just (AstInteger val) -> Just val
    _                     -> Nothing

evalAST :: Ast -> Maybe Ast
evalAST (Define varName value) = do
    evaluatedValue <- evalAST value
    Just (Define varName evaluatedValue)
evalAST (Call func args) = handleCall func args
evalAST (AstInteger n) = Just (AstInteger n)
evalAST (AstFloat f)   = Just (AstFloat f)
evalAST (AstSymbol s)  = Just (AstSymbol s)
evalAST (AstBoolean b) = Just (AstBoolean b)
evalAST (AstList exprs) = do
    astExprs <- mapM evalAST exprs
    Just (AstList astExprs)
