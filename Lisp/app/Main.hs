{-
-- EPITECH PROJECT, 2025
-- Main.hs
-- File description:
-- Main.hs
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Executor (executeLisp, astToString)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            input <- getContents
            processInput input
        (input:_) -> do
            processInput input

processInput :: String -> IO ()
processInput input = do
    case executeLisp input of
        Left err -> do
            hPutStrLn stderr err
            return ()
        Right result -> do
            putStrLn (astToString result)
            return ()

