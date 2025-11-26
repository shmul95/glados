module Executor (
    executeLispWithEnv,
    astToString
) where

import Lisp.AST.AST (Ast(..), evalAST, evalASTWithEnv, Environment)
import Lisp.AST.SExprToAST (sexprToAST)
import Lisp.Parser.Parser (parseLispDocument)
import Lisp.SExpr.SExpr (SExpr(..))
import Text.Megaparsec (parse, errorBundlePretty)

executeLispWithEnv :: Environment -> String -> (Environment, Either String Ast)
executeLispWithEnv env input =
    case parse parseLispDocument "" input of
        Left err -> (env, Left $ "Parse error: " ++ errorBundlePretty err)
        Right (List sexprs) ->
            case mapM sexprToAST sexprs of
                Nothing -> (env, Left "AST conversion error")
                Just asts ->
                    let (newEnv, result) = evalASTWithEnv env asts
                    in case result of
                        Just ast -> (newEnv, Right ast)
                        Nothing -> (newEnv, Left "Evaluation error")
        Right sexpr ->
            case sexprToAST sexpr of
                Nothing -> (env, Left "AST conversion error")
                Just ast ->
                    let (newEnv, result) = evalAST env ast
                    in case result of
                        Just r -> (newEnv, Right r)
                        Nothing -> (newEnv, Left "Evaluation error")


astToString :: Ast -> String
astToString (AstInteger n) = show n
astToString (AstBoolean True) = "#t"
astToString (AstBoolean False) = "#f"
astToString (AstSymbol s) = s
astToString (Lambda _ _ _) = "#<procedure>"
astToString _ = ""
