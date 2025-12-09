{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Rune.Pipelines (compilePipeline, interpretPipeline) where

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import Logger (logError)
import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.Backend.X86_64.Codegen (emitAssembly)
import Rune.IR.Generator (generateIR)
import Rune.IR.Nodes (IRProgram)
import Rune.IR.Printer (prettyPrintIR)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)
-- WARN: removed semantic checking due to not yet working features
-- import Rune.Semantics.Vars (verifVars)
import Text.Megaparsec (errorBundlePretty)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> IO ()
compilePipeline inFile outFile = runPipelineAction inFile (writeFile outFile . emitAssembly)

interpretPipeline :: FilePath -> IO ()
interpretPipeline inFile = runPipelineAction inFile (putStr . prettyPrintIR)

--
-- private pipelines
--

pipeline :: (FilePath, String) -> Either String IRProgram
pipeline =
  parseLexer
    >=> parseAST
    -- WARN: removed semantic checking due to not yet working features
    -- causing erorrs on valid programs
    -- >=> checkSemantics
    >=> genIR

runPipeline :: FilePath -> IO (Either String IRProgram)
runPipeline fp = do
  readContent <- safeRead fp
  pure $ readContent >>= (pipeline . (fp,))

runPipelineAction :: FilePath -> (IRProgram -> IO ()) -> IO ()
runPipelineAction inFile onSuccess =
  runPipeline inFile >>= \case
    Left err -> logError err
    Right ir -> onSuccess ir

--
-- private encapsulations for error handling
--

genIR :: Program -> Either String IRProgram
genIR p = Right $ generateIR p

-- WARN: removed semantic checking due to not yet working features
-- checkSemantics :: Program -> Either String Program
-- checkSemantics p = case verifVars p of
--   Just err -> Left err
--   Nothing -> Right p

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
