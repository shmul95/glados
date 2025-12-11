{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Rune.Pipelines (
  compilePipeline,
  compileToObject,
  interpretPipeline,
  CompileMode (..)
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
import System.FilePath (takeExtension, dropExtension)

data CompileMode
  = ToObject
  | ToExecutable
  | ToAssembly
  | FullCompile
  deriving (Show, Eq)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> CompileMode -> IO ()
compilePipeline inFile outFile FullCompile =
  runPipelineAction inFile (\ir -> do
    let fileWithoutExt = dropExtension inFile
    let asmFile = fileWithoutExt ++ ".asm"
    let objFile = fileWithoutExt ++ ".o"
    writeRuneInAsm asmFile ir
    compileToObject asmFile objFile ir
    compileObjectIntoExecutable objFile outFile
  )
compilePipeline inFile outFile ToAssembly =
  runPipelineAction inFile (\ir -> writeRuneInAsm outFile ir)
compilePipeline inFile outFile ToObject =
  runPipelineAction inFile (\ir -> compileToObject inFile outFile ir)
compilePipeline inFile outFile ToExecutable = compileObjectIntoExecutable inFile outFile

writeRuneInAsm :: FilePath -> IRProgram -> IO ()
writeRuneInAsm asmFile ir = writeFile asmFile (emitAssembly ir)

isRuneSourceFile :: FilePath -> Bool
isRuneSourceFile fp = takeExtension fp == ".ru"

compileToObject :: FilePath -> FilePath -> IRProgram -> IO ()
compileToObject inFile outFile ir = do
  asmFile <- if isRuneSourceFile inFile
      then do
        let baseFile = dropExtension inFile ++ ".asm"
        writeRuneInAsm baseFile ir
        return $ baseFile
      else return inFile
  exitCode <- system $ "nasm -f elf64 " ++ asmFile ++ " -o " ++ outFile
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
