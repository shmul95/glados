{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, hFlush, stdout, stdin, hIsTerminalDevice)
import Executor (executeLispWithEnv, astToString)
import Lisp.AST.AST (Environment)

main :: IO ()
main = getArgs >>= \case
    [] -> hIsTerminalDevice stdin >>= \case
        True -> lispLoop []
        False -> getContents >>= processInput
    [filename] -> readFile filename >>= processInput
    _ -> hPutStrLn stderr "Usage: glados [script.lisp]"

lispLoop :: Environment -> IO ()
lispLoop env = do
    putStr "> "
    hFlush stdout
    getLine >>= handleInput env . trim

handleInput :: Environment -> String -> IO ()
handleInput env = \case
    "" -> lispLoop env
    "exit" -> return ()
    input -> do
        let (newEnv, result) = executeLispWithEnv env input
        either putStrLn (putStrLn . astToString) result
        lispLoop newEnv

processInput :: String -> IO ()
processInput input = do
    let (_, result) = executeLispWithEnv [] input
    either (hPutStrLn stderr) (putStrLn . astToString) result

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")
