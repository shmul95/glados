module Rune.Pipelines (compilePipeline, interpretPipeline) where

import Control.Exception (IOException, try)
import Logger (logError)
import Rune.AST.Nodes (Program)
import Rune.AST.Parser.Parser (parseRune)
import Rune.AST.Printer (prettyPrint)
import Rune.Lexer.Lexer (lexer)
import Text.Megaparsec (errorBundlePretty)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> IO ()
compilePipeline inFile outFile = do
  fileResult <- try (readFile inFile) :: IO (Either IOException String)
  case fileResult of
    Left ex -> logError $ "Failed to read input file: " ++ show ex
    Right content -> processContent inFile content $ \ast ->
      writeFile outFile (prettyPrint ast)

interpretPipeline :: FilePath -> IO ()
interpretPipeline inFile = do
  fileResult <- try (readFile inFile) :: IO (Either IOException String)
  case fileResult of
    Left ex -> logError $ "Failed to read input file: " ++ show ex
    Right content -> processContent inFile content $ \ast ->
      putStrLn (prettyPrint ast)

--
-- private
--

processContent :: FilePath -> String -> (Program -> IO ()) -> IO ()
processContent filename content onSuccess =
  case lexer filename content of
    Left err -> logError (errorBundlePretty err)
    Right tokens -> case parseRune filename tokens of
      Left err -> logError err
      Right ast -> onSuccess ast
