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
    evalAST,
    evalASTWithEnv,
    extractInteger,
    compEnv
) where

import SExpr (SExpr(..))

type Environment = [(String, Ast)]

data Ast = Define String Ast
    | Call String [Ast]
    | Lambda [String] Ast Environment
    | If Ast Ast Ast
    | AstInteger Int
    | AstSymbol String
    | AstBoolean Bool
    | AstList [Ast]
    deriving (Show, Eq)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (List [Symbol "define", List (Symbol funcName : params), body]) = do
    astBody <- sexprToAST body
    let extractParam se = case se of
            Symbol s -> Just s
            _ -> Nothing
    paramNames <- mapM extractParam params
    let lambda = Lambda paramNames astBody []
    Just (Define funcName lambda)
sexprToAST (List [Symbol "define", Symbol varName, valueExpr]) = do
    astValue <- sexprToAST valueExpr
    Just (Define varName astValue)
sexprToAST (List (Symbol "define" : _)) = Nothing
sexprToAST (List [Symbol "lambda", List args, body]) = do
    astValue <- sexprToAST body
    let extractParam se = case se of
            Symbol s -> Just s
            _ -> Nothing
    astArgs <- mapM extractParam args
    Just (Lambda astArgs astValue [])
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
compEnv :: Environment -> String -> Maybe Ast
compEnv [] _ = Nothing
compEnv ((name, value):xs) str = if name == str then Just value else compEnv xs str

extractInteger :: Environment -> Ast -> Maybe Int
extractInteger env ast = case evalAST env ast of
    Just (AstInteger val) -> Just val
    _                     -> Nothing

handleString :: Environment -> String -> Maybe Ast
handleString _ "#t" = Just (AstBoolean True)
handleString _ "#f" = Just (AstBoolean False)
handleString env s = case compEnv env s of
    Just value -> evalAST env value
    _          -> Just (AstSymbol s)

handleCall :: Environment -> String -> [Ast] -> Maybe Ast
handleCall env op [x, y] | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] = do
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
        _ -> Nothing
handleCall _ _ _ = Nothing

handleCondition :: Environment -> Ast -> Ast -> Ast -> Maybe Ast
handleCondition env c t e = do
        evaluatedCond <- evalAST env c
        case evaluatedCond of
            AstBoolean True  -> evalAST env t
            AstBoolean False -> evalAST env e
            _                -> Nothing

evalASTWithEnv :: Environment -> [Ast] -> Maybe Ast
evalASTWithEnv _ [] = Nothing
evalASTWithEnv env [expr] = evalAST env expr
evalASTWithEnv env (Define varName value : exprs) = do
    evaluatedValue <- evalAST env value
    let newEnv = (varName, evaluatedValue) : env
    evalASTWithEnv newEnv exprs

evalASTWithEnv env (expr : exprs) = do
    _ <- evalAST env expr
    evalASTWithEnv env exprs

evalAST :: Environment -> Ast -> Maybe Ast
evalAST _ (Define _ _) = Just (AstSymbol "")
evalAST env (Call func args) = handleCall env func args
evalAST _ (AstInteger n) = Just (AstInteger n)
evalAST env (AstSymbol s) = handleString env s
evalAST _ (AstBoolean b) = Just (AstBoolean b)
evalAST env (If cond thenExpr elseExpr) = handleCondition env cond thenExpr elseExpr
evalAST env (Lambda params body closureEnv) =
    Just (Lambda params body (if null closureEnv then env else closureEnv))
evalAST _ (AstList []) = Just (AstList [])
evalAST env (AstList [expr]) = evalAST env expr
evalAST env (AstList (AstSymbol funcName : args)) =
    case compEnv env funcName of
        Just (Lambda params body closureEnv) -> do
            evaluatedArgs <- mapM (evalAST env) args
            if length params /= length evaluatedArgs
                then Nothing
                else do
                    let bindings = zip params evaluatedArgs
                    let recursiveEnv = (funcName, Lambda params body closureEnv) : bindings ++ closureEnv ++ env
                    evalAST recursiveEnv body
        _ -> case funcName of
            op | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] ->
                handleCall env op args
            _ -> Nothing

evalAST env (AstList (func : args)) = do
    evaluatedFunc <- evalAST env func
    case evaluatedFunc of
        Lambda params body closureEnv -> do
            evaluatedArgs <- mapM (evalAST env) args
            if length params /= length evaluatedArgs
                then Nothing
                else do
                    let bindings = zip params evaluatedArgs
                    let newEnv = bindings ++ closureEnv
                    evalAST newEnv body
        _ -> evalASTWithEnv env (func : args)
