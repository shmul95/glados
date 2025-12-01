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
        Right sexpr -> executeSExpr env sexpr

executeSExpr :: Environment -> SExpr -> (Environment, Either String Ast)
executeSExpr env (List sexprs) =
    case mapM sexprToAST sexprs of
        Nothing -> (env, Left "AST conversion error")
        Just asts -> executeAsts (evalASTWithEnv env asts)
executeSExpr env sexpr =
    case sexprToAST sexpr of
        Nothing -> (env, Left "AST conversion error")
        Just ast -> executeAsts (evalAST env ast)

executeAsts :: (Environment, Maybe Ast) -> (Environment, Either String Ast)
executeAsts (newEnv, result) =
    (newEnv, maybeToEither "Evaluation error" result)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

astToString :: Ast -> String
astToString (AstInteger n) = show n
astToString (AstBoolean True) = "#t"
astToString (AstBoolean False) = "#f"
astToString (AstSymbol s) = s
astToString (Lambda _ _ _) = "#<procedure>"
astToString _ = ""
