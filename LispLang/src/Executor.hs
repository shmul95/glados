module Executor (
    executeLispWithEnv,
    astToString
) where

import Lisp.AST.AST (Ast(..), evalAST, evalASTWithEnv, Environment)
import Lisp.AST.SExprToAST (sexprToAST)
import Lisp.Parser.Parser (parseLispDocument)
import Lisp.SExpr.SExpr (SExpr(..))
import Text.Megaparsec (parse, errorBundlePretty)

executeLispWithEnv :: Environment -> String -> (Environment, Either String [Ast])
executeLispWithEnv env input =
    case parse parseLispDocument "" input of
        Left err -> (env, Left $ "Parse error: " ++ errorBundlePretty err)
        Right sexpr -> executeSExpr env sexpr

executeSExpr :: Environment -> SExpr -> (Environment, Either String [Ast])
executeSExpr env (List sexprs) =
    case mapM sexprToAST sexprs of
        Left err -> (env, Left ("AST conversion error: " ++ err))
        Right asts ->
            let (newEnv, result) = evalASTWithEnv env asts
            in (newEnv, addEvalErrorPrefix result)
executeSExpr env sexpr =
    case sexprToAST sexpr of
        Left err -> (env, Left ("AST conversion error: " ++ err))
        Right ast ->
            let (newEnv, result) = evalAST env ast
            in (newEnv, addEvalErrorPrefix (fmap (:[]) result))

addEvalErrorPrefix :: Either String a -> Either String a
addEvalErrorPrefix (Left err) = Left ("Evaluation error: " ++ err)
addEvalErrorPrefix (Right val) = Right val

astToString :: [Ast] -> String
astToString asts = concatMap singleAstToString asts

singleAstToString :: Ast -> String
singleAstToString (AstInteger n) = show n ++ "\n"
singleAstToString (AstBoolean True) = "#t" ++ "\n"
singleAstToString (AstBoolean False) = "#f" ++ "\n"
singleAstToString (Lambda _ _ _) = "#<procedure>" ++ "\n"
singleAstToString _ = ""
