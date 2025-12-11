{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

#if defined(TESTING_EXPORT)
module Rune.Pipelines
  ( CompileMode (..),
    compilePipeline,
    compileToObject,
    compileAsmIntoObject,
    interpretPipeline,
    pipeline,
    verifAndGenIR,
    runPipeline,
    runPipelineAction,
    optimizeIR,
    genIR,
    checkSemantics,
    safeRead,
    parseLexer,
    parseAST,
  )
where
#else
module Rune.Pipelines
  ( compilePipeline,
    interpretPipeline,
  )
where
#endif

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import Logger (logError)
import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.Backend.X86_64.Codegen (emitAssembly)
import Rune.IR.Generator (generateIR)
import Rune.IR.Nodes (IRProgram)
import Rune.IR.Printer (prettyPrintIR)
import Rune.IR.Optimizer (runIROptimizer)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)
import Rune.Semantics.Vars (verifVars)
import Rune.SanityChecks (performSanityChecks)
import Rune.Semantics.Type (FuncStack)
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
compilePipeline inFile outFile ToObject = case takeExtension inFile of
  ".ru" -> runPipelineAction inFile (\ir -> compileToObject inFile outFile ir)
  ".asm" -> compileAsmIntoObject inFile outFile
  ext -> logError $ "Unsupported file extension: " ++ ext
compilePipeline inFile outFile ToExecutable = compileObjectIntoExecutable inFile outFile

interpretPipeline :: FilePath -> IO ()
interpretPipeline inFile = runPipelineAction inFile (putStr . prettyPrintIR)

--
-- private pipelines
--

pipeline :: (FilePath, String) -> Either String IRProgram
pipeline =
  parseLexer
    >=> parseAST
    >=> verifAndGenIR
    >=> optimizeIR

verifAndGenIR :: Program -> Either String IRProgram
verifAndGenIR p = do
  (checkedAST, funcStack) <- checkSemantics p
  genIR checkedAST funcStack

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

---
--- private methods for compilation steps
---

writeRuneInAsm :: FilePath -> IRProgram -> IO ()
writeRuneInAsm asmFile ir = writeFile asmFile (emitAssembly ir)

compileToObject :: FilePath -> FilePath -> IRProgram -> IO ()
compileToObject inFile outFile ir = do
  let asmFile = dropExtension inFile ++ ".asm"
  writeRuneInAsm asmFile ir
  compileAsmIntoObject asmFile outFile

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

--
-- private encapsulations for error handling
--

optimizeIR :: IRProgram -> Either String IRProgram
optimizeIR p = Right $ runIROptimizer p

genIR :: Program -> FuncStack -> Either String IRProgram
genIR p fs = Right $ generateIR p fs

checkSemantics :: Program -> Either String (Program, FuncStack)
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
