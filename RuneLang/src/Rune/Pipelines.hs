{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Rune.Pipelines (
  compilePipeline,
  writeRuneInAsm,
  compileAsmIntoObject,
  compileObjectIntoExecutable,
  interpretPipeline
) where

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
import Rune.Semantics.Vars (verifVars)
import Rune.SanityChecks (performSanityChecks)
import Text.Megaparsec (errorBundlePretty)
import System.Process (system)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (dropExtension)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> IO ()
compilePipeline inFile outFile =
  runPipelineAction inFile (\ir -> do
    let fileWithoutExt = dropExtension inFile
    let asmFile = fileWithoutExt ++ ".asm"
    let objFile = fileWithoutExt ++ ".o"
    writeRuneInAsm fileWithoutExt ir
    compileAsmIntoObject asmFile objFile
    compileObjectIntoExecutable objFile outFile
  )

writeRuneInAsm :: FilePath -> IRProgram -> IO ()
writeRuneInAsm inFile ir = writeFile (inFile ++ ".asm") (emitAssembly ir)

compileAsmIntoObject :: FilePath -> FilePath -> IO ()
compileAsmIntoObject asmFile objFile = do
  exitCode <- system $ "nasm -f elf64 " ++ asmFile ++ " -o " ++ objFile
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> logError $ "Assembly to object compilation failed with exit code: " ++ show code

compileObjectIntoExecutable :: FilePath -> FilePath -> IO ()
compileObjectIntoExecutable objFile exeFile = do
  exitCode <- system $ "gcc -no-pie " ++ objFile ++ " -o " ++ exeFile
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> logError $ "Object to executable compilation failed with exit code: " ++ show code

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
  sanityCheck <- performSanityChecks
  case sanityCheck of
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
