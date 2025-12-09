module Lisp.AST.SExprToAST (
    extractSymbol,
    extractParams,
    parseDefineFunction,
    parseDefineVariable,
    parseLambda,
    parseIf,
    parseFunctionCall,
    sexprToAST
) where

import Lisp.AST.AST
import Lisp.AST.ASTError
import Lisp.SExpr.SExpr (SExpr(..))

extractSymbol :: SExpr -> Either String String
extractSymbol (Symbol s) = Right s
extractSymbol _ = Left "Expected symbol"

extractParams :: [SExpr] -> Either String [String]
extractParams = mapM extractSymbol

parseDefineFunction :: String -> [SExpr] -> SExpr -> Either String Ast
parseDefineFunction funcName params body = do
    astBody <- sexprToAST body
    paramNames <- extractParams params
    let lambda = Lambda paramNames astBody []
    return $ Define funcName lambda

parseDefineVariable :: String -> SExpr -> Either String Ast
parseDefineVariable varName valueExpr = do
    astValue <- sexprToAST valueExpr
    return $ Define varName astValue

parseLambda :: [SExpr] -> SExpr -> Either String Ast
parseLambda args body = do
    astBody <- sexprToAST body
    astArgs <- extractParams args
    return $ Lambda astArgs astBody []

parseIf :: SExpr -> SExpr -> SExpr -> Either String Ast
parseIf condExpr thenExpr elseExpr = do
    astCond <- sexprToAST condExpr
    astThen <- sexprToAST thenExpr
    astElse <- sexprToAST elseExpr
    return $ If astCond astThen astElse

parseFunctionCall :: SExpr -> [SExpr] -> Either String Ast
parseFunctionCall funcExpr argExprs = do
    astFunc <- sexprToAST funcExpr
    astArgs <- mapM sexprToAST argExprs
    case astFunc of
        AstSymbol op | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] ->
            return $ Call op astArgs
        _ -> return $ AstList (astFunc : astArgs)

isReservedSymbol :: String -> Bool
isReservedSymbol s = s `elem` ["define", "lambda", "if"]

sexprToAST :: SExpr -> Either String Ast
sexprToAST (Integer n) = Right (AstInteger n)
sexprToAST (Symbol s)
    | isReservedSymbol s = Left (reservedSymbolError s)
    | otherwise = Right (AstSymbol s)
sexprToAST (List sexprs) = parseList sexprs

parseList :: [SExpr] -> Either String Ast
parseList [] = Left emptySExprError
parseList [Symbol "define", List (Symbol funcName : params), body] =
    parseDefineFunction funcName params body
parseList [Symbol "define", Symbol varName, valueExpr] =
    parseDefineVariable varName valueExpr
parseList (Symbol "define" : _) = Left invalidDefineError
parseList [Symbol "lambda", List args, body] =
    parseLambda args body
parseList (Symbol "lambda" : _) = Left invalidLambdaError
parseList [Symbol "if", condExpr, thenExpr, elseExpr] =
    parseIf condExpr thenExpr elseExpr
parseList (Symbol "if" : _) = Left invalidIfError
parseList (funcExpr : argExprs) =
    parseFunctionCall funcExpr argExprs
