{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Rune.Pipelines (compilePipeline, interpretPipeline) where

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import Logger (logError)
import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.AST.Printer (prettyPrint)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)
import Text.Megaparsec (errorBundlePretty)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> IO ()
compilePipeline inFile outFile = runPipelineAction inFile (writeFile outFile . prettyPrint)

interpretPipeline :: FilePath -> IO ()
interpretPipeline inFile = runPipelineAction inFile (putStrLn . prettyPrint)

--
-- private pipelines
--

pipeline :: (FilePath, String) -> Either String Program
pipeline = parseLexer >=> parseAST

-- >=> analyzeSemantics
-- >=> createIR
-- >=> optimizeIR
-- >=> generateCode

runPipeline :: FilePath -> IO (Either String Program)
runPipeline fp = do
  readContent <- safeRead fp
  pure $ readContent >>= (pipeline . (fp,))

runPipelineAction :: FilePath -> (Program -> IO ()) -> IO ()
runPipelineAction inFile onSuccess =
  runPipeline inFile >>= \case
    Left err -> logError err
    Right ast -> onSuccess ast

--
-- private encapsulations for error handling
--

safeRead :: FilePath -> IO (Either String String)
safeRead fp = do
  r <- try (readFile fp) :: IO (Either IOException String)
  pure $ case r of
    Left ex -> Left $ "Failed to read input file: " ++ show ex
    Right content -> Right content

parseLexer :: (FilePath, String) -> Either String (FilePath, [Token])
parseLexer (fp, content) =
  case lexer fp content of
    Left err -> Left $ errorBundlePretty err
    Right tokens -> Right (fp, tokens)

parseAST :: (FilePath, [Token]) -> Either String Program
parseAST (fp, tokens) =
  case parseRune fp tokens of
    Left err -> Left err
    Right ast -> Right ast
