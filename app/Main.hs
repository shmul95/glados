{-
-- EPITECH PROJECT, 2025
-- Main.hs
-- File description:
-- Main.hs
-}

module Main (main) where

-- import CLI (parseArgs, runCLI)
-- import Logger (logError)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (parse, errorBundlePretty)
import Parser (parseLispDocument)
import SExpr (printTree)

main :: IO ()
-- main = getArgs >>= either logError runCLI . parseArgs
main = do
    args <- getArgs
    case args of
        [] -> do
            hPutStrLn stderr "Usage: glados-exe <input>"
            return ()
        (input:_) -> do
            case parse parseLispDocument "" input of
                Left err -> do
                    hPutStrLn stderr $ "Parse error: " ++ errorBundlePretty err
                    return ()
                Right sexpr -> do
                    case printTree sexpr of
                        Just output -> putStrLn output
                        Nothing -> putStrLn "Error: Could not print tree"

