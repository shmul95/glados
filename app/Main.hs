module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (parse)
import Parser (parseLispDocument)
import SExpr (printTree)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            hPutStrLn stderr "Usage: glados-exe <input>"
            return ()
        (input:_) -> do
            case parse parseLispDocument "" input of
                Left err -> do
                    hPutStrLn stderr $ "Parse error: " ++ show err
                    return ()
                Right sexpr -> do
                    case printTree sexpr of
                        Just output -> putStrLn output
                        Nothing -> putStrLn "Error: Could not print tree"
