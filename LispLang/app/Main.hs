{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr, hFlush, stdout, stdin, hIsTerminalDevice)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, IOException)
import Executor (executeLispWithEnv, astToString)
import Lisp.AST.AST (Environment)

main :: IO ()
main = getArgs >>= \case
    [] -> hIsTerminalDevice stdin >>= \case
        True -> lispLoop []
        False -> getContents >>= processInput
    filenames -> processMultipleFiles filenames

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
        either putStrLn (putStr . astToString) result
        lispLoop newEnv

processInput :: String -> IO ()
processInput input = do
    let (_, result) = executeLispWithEnv [] input
    either (hPutStrLn stderr) (putStr . astToString) result

processMultipleFiles :: [FilePath] -> IO ()
processMultipleFiles filenames = do
    result <- readAllFiles filenames
    case result of
        Left err -> hPutStr stderr err >> exitWith (ExitFailure 84)
        Right concatenatedContent -> processInput concatenatedContent

readAllFiles :: [FilePath] -> IO (Either String String)
readAllFiles [] = return $ Right ""
readAllFiles filenames = do
    results <- mapM readFileSafe filenames
    let errors = [err | Left err <- results]
    if not (null errors)
        then return $ Left (unlines errors)
        else return $ Right (concat [content | Right content <- results])

readFileSafe :: FilePath -> IO (Either String String)
readFileSafe filename =
    (Right <$> readFile filename) `catch`
        (\e -> return $ Left $ "Error reading file " ++ filename ++ ": " ++ show (e :: IOException))

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")
