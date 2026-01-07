{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

#if defined(TESTING_EXPORT)
module Rune.Pipelines
  ( CompileMode (..),
    compilePipeline,
    compileMultiplePipeline,
    compileAsmToObject,
    compileObjectIntoExecutable,
    linkObjectsIntoExecutable,
    emitAssembly,
    interpretPipeline,
    pipeline,
    verifAndGenIR,
    runPipeline,
    runPipelineAction,
    optimizeIR,
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
    compileMultiplePipeline,
  )
where
#endif

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (IOException, try, bracket)
import Control.Monad ((>=>), when)

import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe (catMaybes)

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

import Lib (fixpoint)

import Text.Megaparsec (errorBundlePretty)
import System.Process (rawSystem)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeExtension, dropExtension)
import System.IO (hPutStr, hClose, openTempFile)
import System.Directory (removeFile, getTemporaryDirectory)

data CompileMode
  = ToObject
  | ToExecutable
  | ToAssembly
  | FullCompile
  deriving (Show, Eq)

--
-- public
--

--
-- compiler pipeline routine
--

-- | rune build <file.ru> -o output [options]
compilePipeline :: FilePath -> FilePath -> CompileMode -> IO ()
compilePipeline inFile outFile mode =
  case mode of
    FullCompile  -> compileFullPipeline inFile outFile
    ToAssembly   -> compileToAssembly inFile outFile
    ToObject     -> compileToObject inFile outFile
    ToExecutable -> compileObjectIntoExecutable inFile outFile

-- | rune build <file.ru> -o output
compileFullPipeline :: FilePath -> FilePath -> IO ()
compileFullPipeline inFile outFile =
  runPipelineAction inFile $ \ir -> do
    let objFile = dropExtension inFile <> ".o"
        asmContent = emitAssembly ir
    compileAsmToObject asmContent objFile
    compileObjectIntoExecutable objFile outFile

-- | rune build <file.ru> -S -o output.asm
compileToAssembly :: FilePath -> FilePath -> IO ()
compileToAssembly inFile outFile =
  runPipelineAction inFile (writeFile outFile . emitAssembly)

-- | rune build <file.ru|file.asm> -o output.o
compileToObject :: FilePath -> FilePath -> IO ()
compileToObject inFile outFile =
  case takeExtension inFile of
    ".ru" -> runPipelineAction inFile $ \ir ->
              let asmContent = emitAssembly ir
               in compileAsmToObject asmContent outFile
    ".asm" -> safeRead inFile >>= either logError (`compileAsmToObject` outFile)
    ext -> logError $ "Unsupported file extension: " <> ext

-- | rune build <file1.ru> <file2.ru> ... -o output
compileMultiplePipeline :: [FilePath] -> FilePath -> IO ()
compileMultiplePipeline [] _ = logError "No input files provided."
compileMultiplePipeline inFiles outFile = do
  let (runeFiles, remainder) = partition (\fp -> takeExtension fp == ".ru") inFiles
      (asmFiles, objectFiles) = partition (\fp -> takeExtension fp == ".asm") remainder

  runeObjs <- compileRuneSources runeFiles outFile
  asmObjs  <- compileAsmSources asmFiles
  let allObjects = objectFiles <> asmObjs <> runeObjs
  case allObjects of
    [] -> logError "No object files to link."
    _  -> linkObjectsIntoExecutable allObjects outFile

-- | compile many Rune sources files into object files concurrently
-- rune build <file1.ru> <file2.ru> ... -o output
--
-- threads:
--  1. compile each file separately in parallel
--  2. return list of object files
--  3. log errors as they occur
compileRuneSources :: [FilePath] -> FilePath -> IO [FilePath]
compileRuneSources [] _ = pure []
compileRuneSources runeFiles _ = do
  performSanityChecks >>= either (\e -> logError e >> pure []) (\() -> do
    results <- mapConcurrently compileRuneFile runeFiles
    pure (catMaybes results)
    )
  where
    compileRuneFile :: FilePath -> IO (Maybe FilePath)
    compileRuneFile runeFile = do
      result <- runPipeline runeFile
      case result of
        Left err -> logError err >> pure Nothing
        Right ir -> do
          let objFile = dropExtension runeFile <> ".o"
              asmContent = emitAssembly ir
          compileAsmToObject asmContent objFile
          pure (Just objFile)

-- | compile a list of assembly files into object files concurrently
compileAsmSources :: [FilePath] -> IO [FilePath]
compileAsmSources asmFiles = do
  let targets = map (\fp -> (fp, dropExtension fp <> ".o")) asmFiles
  results <- mapConcurrently compileTarget targets
  pure (catMaybes results)
  where
    compileTarget (asmFile, objFile) =
      safeRead asmFile >>= either (\e -> logError e >> pure Nothing) (\asmContent -> compileAsmToObject asmContent objFile >> pure (Just objFile))

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
  generateIR checkedAST funcStack

runPipeline :: FilePath -> IO (Either String IRProgram)
runPipeline fp = do
  performSanityChecks >>= either (pure . Left)
    (\() -> safeRead fp <&> (>>= (pipeline . (fp,))))


runPipelineAction :: FilePath -> (IRProgram -> IO ()) -> IO ()
runPipelineAction inFile onSuccess =
  runPipeline inFile >>= either logError onSuccess

---
--- private methods for compilation steps
---

compileAsmToObject :: String -> FilePath -> IO ()
compileAsmToObject asmContent objFile = do
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "asm-XXXXXX.asm")
          (\(asmFile, h) -> hClose h >> removeFile asmFile)
          (\(asmFile, h) -> do
              hPutStr h asmContent >> hClose h
              exitCode <- rawSystem "nasm" ["-f", "elf64", asmFile, "-o", objFile]
              when (exitCode /= ExitSuccess) $
                logError $ "Assembly to object compilation failed with exit code: " <> show exitCode)

compileObjectIntoExecutable :: FilePath -> FilePath -> IO ()
compileObjectIntoExecutable objFile = linkObjectsIntoExecutable [objFile]

linkObjectsIntoExecutable :: [FilePath] -> FilePath -> IO ()
linkObjectsIntoExecutable objFiles exeFile = do
  exitCode <- rawSystem "gcc" (("-no-pie":) $ objFiles ++ ["-o", exeFile])
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> logError $ "Object to executable compilation failed with exit code: " <> show code

--
-- private encapsulations for error handling
--

optimizeIR :: IRProgram -> Either String IRProgram
optimizeIR = Right . fixpoint runIROptimizer

checkSemantics :: Program -> Either String (Program, FuncStack)
checkSemantics = verifVars

safeRead :: FilePath -> IO (Either String String)
safeRead fp = do
  r <- try (readFile fp) :: IO (Either IOException String)
  pure $ either (Left . ("Failed to read input file: " <>) . show) Right r

parseLexer :: (FilePath, String) -> Either String (FilePath, [Token])
parseLexer (fp, content) = either (Left . errorBundlePretty) (Right . (fp,)) (lexer fp content)

parseAST :: (FilePath, [Token]) -> Either String Program
parseAST (fp, tokens) = Right =<< parseRune fp tokens
