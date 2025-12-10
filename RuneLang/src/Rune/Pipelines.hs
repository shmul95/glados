{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Rune.Pipelines (compilePipeline, interpretPipeline) where

import Control.Exception (IOException, try)
import Control.Monad ((>=>), unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans(lift)
import System.Directory (findExecutable)
import Data.Maybe (isJust)
import Logger (logError)
import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.Backend.X86_64.Codegen (emitAssembly)
import Rune.IR.Generator (generateIR)
import Rune.IR.Nodes (IRProgram)
import Rune.IR.Printer (prettyPrintIR)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)
import Rune.Semantics.Vars (verifVars)
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
    >=> checkSemantics
    >=> genIR

runPipeline :: FilePath -> IO (Either String IRProgram)
runPipeline fp = do
  toolSanity <- checkToolchainSanity
  case toolSanity of
    Left err -> pure $ Left err
    Right () -> do
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

hasTool :: String -> IO Bool
hasTool = fmap isJust . findExecutable

checkToolchainSanityWith :: (String -> IO Bool) -> IO (Either String ())
checkToolchainSanityWith hasTool' = runExceptT $ do
  gcc <- lift $ hasTool' "gcc"
  nasm <- lift $ hasTool' "nasm"
  unless gcc $ throwError "'gcc' not found in PATH. Please install GCC to proceed."
  unless nasm $ throwError "'nasm' not found in PATH. Please install NASM to proceed."

checkToolchainSanity :: IO (Either String ())
checkToolchainSanity = checkToolchainSanityWith hasTool

genIR :: Program -> Either String IRProgram
genIR p = Right $ generateIR p

checkSemantics :: Program -> Either String Program
checkSemantics = verifVars

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
