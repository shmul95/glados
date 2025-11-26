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
import Lisp.SExpr.SExpr (SExpr(..))

extractSymbol :: SExpr -> Maybe String
extractSymbol (Symbol s) = Just s
extractSymbol _          = Nothing

extractParams :: [SExpr] -> Maybe [String]
extractParams = mapM extractSymbol

parseDefineFunction :: String -> [SExpr] -> SExpr -> Maybe Ast
parseDefineFunction funcName params body = do
    astBody <- sexprToAST body
    paramNames <- extractParams params
    let lambda = Lambda paramNames astBody []
    return $ Define funcName lambda

parseDefineVariable :: String -> SExpr -> Maybe Ast
parseDefineVariable varName valueExpr = do
    astValue <- sexprToAST valueExpr
    return $ Define varName astValue

parseLambda :: [SExpr] -> SExpr -> Maybe Ast
parseLambda args body = do
    astBody <- sexprToAST body
    astArgs <- extractParams args
    return $ Lambda astArgs astBody []

parseIf :: SExpr -> SExpr -> SExpr -> Maybe Ast
parseIf condExpr thenExpr elseExpr = do
    astCond <- sexprToAST condExpr
    astThen <- sexprToAST thenExpr
    astElse <- sexprToAST elseExpr
    return $ If astCond astThen astElse

parseFunctionCall :: SExpr -> [SExpr] -> Maybe Ast
parseFunctionCall funcExpr argExprs = do
    astFunc <- sexprToAST funcExpr
    astArgs <- mapM sexprToAST argExprs
    case astFunc of
        AstSymbol op | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] ->
            return $ Call op astArgs
        _ -> return $ AstList (astFunc : astArgs)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s) = Just (AstSymbol s)
sexprToAST (List [Symbol "define", List (Symbol funcName : params), body]) =
    parseDefineFunction funcName params body
sexprToAST (List [Symbol "define", Symbol varName, valueExpr]) =
    parseDefineVariable varName valueExpr
sexprToAST (List (Symbol "define" : _)) = Nothing
sexprToAST (List [Symbol "lambda", List args, body]) =
    parseLambda args body
sexprToAST (List (Symbol "lambda" : _)) = Nothing
sexprToAST (List [Symbol "if", condExpr, thenExpr, elseExpr]) =
    parseIf condExpr thenExpr elseExpr
sexprToAST (List (funcExpr : argExprs)) =
    parseFunctionCall funcExpr argExprs
sexprToAST (List exprs) =
    fmap AstList (mapM sexprToAST exprs)
