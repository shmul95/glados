module Lisp.AST.AST (
    Ast(..),
    handleCall,
    handleString,
    evalAST,
    evalASTWithEnv,
    extractInteger,
    compEnv,
    Environment
) where

import Lisp.AST.ASTError

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

compEnv :: Environment -> String -> Maybe Ast
compEnv [] _ = Nothing
compEnv ((name, value):xs) str
    | name == str = Just value
    | otherwise = compEnv xs str

extractInteger :: Environment -> Ast -> Maybe Int
extractInteger env ast =
    case evalAST env ast of
        (_, Right (AstInteger n)) -> Just n
        _ -> Nothing

handleString :: Environment -> String -> (Environment, Either String Ast)
handleString env "#t" = (env, Right (AstBoolean True))
handleString env "#f" = (env, Right (AstBoolean False))
handleString env s =
    case compEnv env s of
        Just value -> evalAST env value
        _          -> (env, Left (undefinedVariableError s))

extractIntegerOrError :: Environment -> Ast -> Either String Int
extractIntegerOrError env ast =
    case extractInteger env ast of
        Just n -> Right n
        Nothing -> Left argumentMustBeIntegerError

guardNonZero :: Int -> Ast -> Either String Ast
guardNonZero b result = if b /= 0
    then Right result
    else Left divisionByZeroError

applyOp :: String -> Int -> Int -> Either String Ast
applyOp op a b = case op of
    "+" -> Right $ AstInteger (a + b)
    "-" -> Right $ AstInteger (a - b)
    "*" -> Right $ AstInteger (a * b)
    "div" -> guardNonZero b (AstInteger (a `div` b))
    "mod" -> guardNonZero b (AstInteger (a `mod` b))
    "eq?" -> Right $ AstBoolean (a == b)
    "<" -> Right $ AstBoolean (a < b)
    _ -> Left (unknownOperatorError op)

validateArgCount :: Int -> [Ast] -> Either String [Ast]
validateArgCount n args = if length args == n
    then Right args
    else Left operatorRequiresTwoArgumentsError

handleOpt :: Environment -> String -> [Ast] -> Either String Ast
handleOpt env op args = do
    case validateArgCount 2 args of
        Left err -> Left err
        Right [x, y] -> do
            a <- extractIntegerOrError env x
            b <- extractIntegerOrError env y
            applyOp op a b
        Right _ -> Left operatorRequiresTwoArgumentsError

evaluateArg :: Environment -> Ast -> Ast
evaluateArg env arg = case evalAST env arg of
    (_, Right val) -> val
    (_, Left _)    -> AstSymbol ""

handleCall :: Environment -> String -> [String] -> Ast -> Environment -> [Ast]
    -> (Environment, Either String Ast)
handleCall env funcName params body closureEnv args =
    case validateArgCount (length params) args of
        Left err -> (env, Left err)
        Right _ ->
            let bindings = zip params (map (evaluateArg env) args)
                funcBinding = (funcName, Lambda params body closureEnv)
                newEnv = funcBinding : bindings ++ closureEnv
            in evalAST newEnv body

handleCondition :: Environment -> Ast -> Ast -> Ast
    -> (Environment, Either String Ast)
handleCondition env c t e =
    let (_, evaluatedCond) = evalAST env c
    in case evaluatedCond of
        Right (AstBoolean True)  -> evalAST env t
        Right (AstBoolean False) -> evalAST env e
        _                       -> (env, Left conditionMustBeBooleanError)

handleDefine :: Environment -> String -> Ast -> (Environment, Either String Ast)
handleDefine env name value =
    let (_, evaluatedValue) = evalAST env value
    in case evaluatedValue of
        Right val -> ((name, val) : env, Right (AstSymbol ""))
        Left err  -> (env, Left err)

evalASTWithEnv :: Environment -> [Ast] -> (Environment, Either String [Ast])
evalASTWithEnv env [] = (env, Right [])
evalASTWithEnv env (expr:rest) =
    case evalAST env expr of
        (newEnv, Left err) -> (newEnv, Left err)
        (newEnv, Right res) ->
            case evalASTWithEnv newEnv rest of
                (finalEnv, Left err) -> (finalEnv, Left err)
                (finalEnv, Right restRes) -> (finalEnv, Right (res : restRes))

evalAST :: Environment -> Ast -> (Environment, Either String Ast)
evalAST env (AstInteger n) = (env, Right (AstInteger n))
evalAST env (AstBoolean b) = (env, Right (AstBoolean b))
evalAST env (AstSymbol s) = handleString env s
evalAST env (Call op args) = (env, handleOpt env op args)
evalAST env (If cond thenExpr elseExpr) =
    handleCondition env cond thenExpr elseExpr
evalAST env (Lambda params body _) = (env, Right (Lambda params body env))
evalAST env (Define name value) = handleDefine env name value
evalAST env (AstList (AstSymbol funcName : args)) =
    case compEnv env funcName of
        Just (Lambda params body closureEnv) ->
            handleCall env funcName params body closureEnv args
        _ -> (env, handleOpt env funcName args)
evalAST env (AstList [expr]) = evalAST env expr
evalAST env (AstList []) = (env, Right (AstList []))
evalAST env (AstList _) = (env, Left invalidListExpressionError)