{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, hFlush, stdout, stdin, hIsTerminalDevice)
import Executor (executeLispWithEnv, astToString)
import Lisp.AST.AST (Environment)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            isTerminal <- hIsTerminalDevice stdin
            if isTerminal
                then lispLoop []
                else do
                    input <- getContents
                    processInput input
        [filename] -> readFile filename >>= processInput
        _ -> hPutStrLn stderr "Usage: glados [script.lisp]"

lispLoop :: Environment -> IO ()
lispLoop env = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case trim line of
        "" -> lispLoop env
        "exit" -> return ()
        input -> do
            let (newEnv, result) = executeLispWithEnv env input
            case result of
                Left err -> putStrLn err >> lispLoop newEnv
                Right ast -> do
                    let output = astToString ast
                    unless (null output) (putStrLn output)
                    lispLoop newEnv

processInput :: String -> IO ()
processInput input = do
    let (_, result) = executeLispWithEnv [] input
    case result of
        Left err -> hPutStrLn stderr err
        Right ast -> putStrLn (astToString ast)

unless :: Bool -> IO () -> IO ()
unless True _ = return ()
unless False action = action

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")
