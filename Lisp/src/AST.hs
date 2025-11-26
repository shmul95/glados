module AST (
    Ast(..),
    sexprToAST,
    handleCall,
    handleString,
    evalAST,
    evalASTWithEnv,
    extractInteger,
    compEnv,
    Environment
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
compEnv ((name, value):xs) str
    | name == str = Just value
    | otherwise = compEnv xs str


extractInteger :: Environment -> Ast -> Maybe Int
extractInteger  env ast = case evalAST env ast of
    (_, Just (AstInteger n)) -> Just n
    _ -> Nothing


handleString :: Environment -> String -> (Environment, Maybe Ast)
handleString env "#t" = (env, Just (AstBoolean True))
handleString env "#f" = (env, Just (AstBoolean False))
handleString env s = case compEnv env s of
    Just value -> evalAST env value
    _          -> (env, Nothing)


handleOpt :: Environment -> String -> [Ast] -> Maybe Ast
handleOpt env op [x, y] =
    case (extractInteger env x, extractInteger env y) of
        (Just a, Just b) -> case op of
            "+" -> Just (AstInteger (a + b))
            "-" -> Just (AstInteger (a - b))
            "*" -> Just (AstInteger (a * b))
            "div" -> if b /= 0 then Just (AstInteger (a `div` b)) else Nothing
            "mod" -> if b /= 0 then Just (AstInteger (a `mod` b)) else Nothing
            "eq?" -> Just (AstBoolean (a == b))
            "<" -> Just (AstBoolean (a < b))
            _ -> Nothing
        _ -> Nothing
handleOpt _ _ _ = Nothing


handleCondition :: Environment -> Ast -> Ast -> Ast -> (Environment, Maybe Ast)
handleCondition env c t e =
        let (_, evaluatedCond) = evalAST env c
        in case evaluatedCond of
            Just (AstBoolean True)  -> evalAST env t
            Just (AstBoolean False) -> evalAST env e
            _                -> (env, Nothing)


handleDefine :: Environment -> String -> Ast -> (Environment, Maybe Ast)
handleDefine env name value =
    let (_, evaluatedValue) = evalAST env value
    in case evaluatedValue of
        Just val -> ((name, val) : env, Just (AstSymbol ""))
        Nothing  -> (env, Nothing)


handleCall :: Environment -> String -> [String] -> Ast -> Environment -> [Ast] -> (Environment, Maybe Ast)
handleCall env funcName params body closureEnv args
    | length params /= length args = (env, Nothing)
    | otherwise =
        let argVals = [case evalAST env arg of (_, Just val) -> val; _ -> AstInteger 0 | arg <- args]
            bindings = zip params argVals
            funcBinding = (funcName, Lambda params body closureEnv)
            newEnv = funcBinding : bindings ++ closureEnv
        in evalAST newEnv body

evalASTWithEnv :: Environment -> [Ast] -> (Environment, Maybe Ast)
evalASTWithEnv env [] = (env, Nothing)
evalASTWithEnv env [expr] = evalAST env expr
evalASTWithEnv env (expr:exprs) =
    let (newEnv, _) = evalAST env expr
    in evalASTWithEnv newEnv exprs


evalAST :: Environment -> Ast -> (Environment, Maybe Ast)
evalAST env (AstInteger n) = (env, Just (AstInteger n))
evalAST env (AstBoolean b) = (env, Just (AstBoolean b))
evalAST env (AstSymbol s) = handleString env s
evalAST env (Call op args) = (env, handleOpt env op args)
evalAST env (If cond thenExpr elseExpr) = handleCondition env cond thenExpr elseExpr
evalAST env (Lambda params body _) = (env, Just (Lambda params body env))
evalAST env (Define name value) = handleDefine env name value
evalAST env (AstList (AstSymbol funcName : args)) =
    case compEnv env funcName of
        Just (Lambda params body closureEnv) -> handleCall env funcName params body closureEnv args
        _ -> (env, handleOpt env funcName args)
evalAST env (AstList [expr]) = evalAST env expr
evalAST env (AstList []) = (env, Just (AstList []))
evalAST env (AstList _) = (env, Nothing)