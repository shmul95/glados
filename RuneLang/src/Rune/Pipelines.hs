{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

#if defined(TESTING_EXPORT)
module Rune.Pipelines
  ( CompileMode (..),
    LibraryOptions (..),
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
    CompileMode (..),
    LibraryOptions (..)
  )
where
#endif

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (IOException, try, bracket)
import Control.Monad ((>=>), when, unless)

import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe (catMaybes)

import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, hPutStr, hClose, openTempFile, stderr)

import Logger (logError)

import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.Backend.X86_64.Codegen (emitAssembly, emitAssemblyLib)
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
import System.FilePath (takeExtension, dropExtension)
import System.Directory (removeFile, getTemporaryDirectory, getHomeDirectory)

data LibraryOptions = LibraryOptions
  { libShared :: Bool
  , libStatic :: Bool
  , libPaths  :: [FilePath]
  , libNames  :: [String]
  } deriving (Show, Eq)

data CompileMode
  = ToObject
  | ToExecutable LibraryOptions
  | ToAssembly
  | FullCompile LibraryOptions
  deriving (Show, Eq)

defaultLibPaths :: FilePath -> [FilePath]
defaultLibPaths home = ["/usr/local/lib", "/usr/lib", home <> "/.local/lib", "./lib/std"]

rpathArg :: FilePath -> String
rpathArg home = "-Wl,-rpath," <> home <> "/.local/lib"

compilePipeline :: FilePath -> FilePath -> CompileMode -> IO ()
compilePipeline = modeToAction
  where
    modeToAction inf outf = \case
      FullCompile libOpts  -> compileFullPipeline inf outf libOpts
      ToAssembly           -> compileToAssembly inf outf
      ToObject             -> compileToObject inf outf
      ToExecutable libOpts -> compileObjectIntoExecutable inf outf libOpts

compileFullPipeline :: FilePath -> FilePath -> LibraryOptions -> IO ()
compileFullPipeline inFile outFile libOpts =
  runPipelineAction inFile $ \ir -> do
    let objFile = objectFileName inFile
        asmContent = selectEmitter libOpts ir
    compileAsmToObject asmContent objFile (isPIC libOpts)
    linkOrArchive libOpts [objFile] outFile

compileToAssembly :: FilePath -> FilePath -> IO ()
compileToAssembly inFile outFile = 
  runPipelineAction inFile (writeFile outFile . emitAssembly)

compileToObject :: FilePath -> FilePath -> IO ()
compileToObject inFile outFile = case takeExtension inFile of
  ".ru"  -> runPipelineAction inFile $ \ir -> 
              compileAsmToObject (emitAssembly ir) outFile False
  ".asm" -> safeRead inFile >>= either logError (\c -> compileAsmToObject c outFile False)
  ext    -> logError $ "Unsupported file extension: " <> ext

compileMultiplePipeline :: [FilePath] -> FilePath -> LibraryOptions -> IO ()
compileMultiplePipeline [] _ _ = logError "No input files provided."
compileMultiplePipeline inFiles outFile libOpts = do
  let (runeFiles, remainder) = partitionByExt ".ru" inFiles
      (asmFiles, objectFiles) = partitionByExt ".asm" remainder
      isLib = isPIC libOpts

  runeObjs <- compileRuneSources runeFiles isLib
  asmObjs  <- compileAsmSources asmFiles isLib
  
  linkOrArchive' libOpts (objectFiles <> asmObjs <> runeObjs) outFile

compileRuneSources :: [FilePath] -> Bool -> IO [FilePath]
compileRuneSources [] _ = pure []
compileRuneSources runeFiles isLib =
  performSanityChecks >>= either exitWithError (const $ compileFiles runeFiles isLib)
  where
    exitWithError e = logError e >> pure []
    compileFiles files forLib = do
      results <- mapConcurrently (compileRuneFile forLib) files
      let (errors, successes) = partitionEithers results
      mapM_ logErrorNoExit errors
      unless (null errors) $ exitWith (ExitFailure 84)
      pure successes

compileRuneFile :: Bool -> FilePath -> IO (Either String FilePath)
compileRuneFile forLib runeFile =
  runPipeline runeFile >>= either (pure . Left) (compileToObj forLib runeFile)
  where
    compileToObj isLib rf ir = do
      let objFile = objectFileName rf
          asmContent = if isLib then emitAssemblyLib ir else emitAssembly ir
      compileAsmToObject asmContent objFile isLib
      pure $ Right objFile

compileAsmSources :: [FilePath] -> Bool -> IO [FilePath]
compileAsmSources asmFiles isLib =
  catMaybes <$> mapConcurrently (compileAsmFile isLib) asmFiles
  where
    compileAsmFile forLib asmFile = do
      let objFile = objectFileName asmFile
      safeRead asmFile >>= either handleError (compileAndReturn objFile forLib)
    
    handleError e = logError e >> pure Nothing
    compileAndReturn obj forLib content = 
      compileAsmToObject content obj forLib >> pure (Just obj)

interpretPipeline :: FilePath -> IO ()
interpretPipeline inFile = runPipelineAction inFile (putStr . prettyPrintIR)

pipeline :: (FilePath, String) -> Either String IRProgram
pipeline = parseLexer >=> parseAST >=> verifAndGenIR >=> optimizeIR

verifAndGenIR :: Program -> Either String IRProgram
verifAndGenIR = checkSemantics >=> uncurry generateIR

runPipeline :: FilePath -> IO (Either String IRProgram)
runPipeline fp = performSanityChecks >>= either (pure . Left) 
  (const $ safeRead fp <&> (>>= pipeline . (fp,)))

runPipelineAction :: FilePath -> (IRProgram -> IO ()) -> IO ()
runPipelineAction inFile = (runPipeline inFile >>=) . either logError

compileAsmToObject :: String -> FilePath -> Bool -> IO ()
compileAsmToObject asmContent objFile forPic = 
  withTempAsm asmContent (if forPic then assembleToObjectPIC objFile else assembleToObject objFile)

withTempAsm :: String -> (FilePath -> IO ()) -> IO ()
withTempAsm content action = do
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "asm-XXXXXX.asm")
          (\(asmFile, h) -> hClose h >> removeFile asmFile)
          (\(asmFile, h) -> hPutStr h content >> hClose h >> action asmFile)

assembleToObject :: FilePath -> FilePath -> IO ()
assembleToObject objFile asmFile = do
  exitCode <- rawSystem "nasm" ["-f", "elf64", asmFile, "-o", objFile]
  when (exitCode /= ExitSuccess) $
    logError $ "Assembly to object compilation failed with exit code: " <> show exitCode

assembleToObjectPIC :: FilePath -> FilePath -> IO ()
assembleToObjectPIC objFile asmFile = do
  exitCode <- rawSystem "nasm" ["-f", "elf64", "-DPIC", asmFile, "-o", objFile]
  when (exitCode /= ExitSuccess) $
    logError $ "Assembly to object compilation failed with exit code: " <> show exitCode

compileObjectIntoExecutable :: FilePath -> FilePath -> LibraryOptions -> IO ()
compileObjectIntoExecutable objFile outFile libOpts = 
  linkOrArchive libOpts [objFile] outFile

linkOrArchive :: LibraryOptions -> [FilePath] -> FilePath -> IO ()
linkOrArchive libOpts objFiles outFile
  | libStatic libOpts = createStaticLibrary objFiles outFile
  | libShared libOpts = createSharedLibrary objFiles outFile libOpts
  | otherwise         = linkObjectsIntoExecutable objFiles outFile libOpts

linkOrArchive' :: LibraryOptions -> [FilePath] -> FilePath -> IO ()
linkOrArchive' _ [] _ = logError "No object files to link."
linkOrArchive' libOpts objs out = linkOrArchive libOpts objs out

createStaticLibrary :: [FilePath] -> FilePath -> IO ()
createStaticLibrary objFiles outFile = 
  runCommand "ar" (["rcs", outFile] ++ objFiles) 
    "Static library creation failed with exit code: "

createSharedLibrary :: [FilePath] -> FilePath -> LibraryOptions -> IO ()
createSharedLibrary objFiles outFile libOpts = do
  home <- getHomeDirectory
  let args = sharedLibArgs home objFiles outFile libOpts
  runCommand "gcc" args "Shared library creation failed with exit code: "

sharedLibArgs :: FilePath -> [FilePath] -> FilePath -> LibraryOptions -> [String]
sharedLibArgs home objFiles outFile libOpts =
  ["-shared", "-fPIC", "-o", outFile] 
  ++ objFiles 
  ++ libPathArgs home libOpts
  ++ libNameArgs libOpts
  ++ [rpathArg home]

linkObjectsIntoExecutable :: [FilePath] -> FilePath -> LibraryOptions -> IO ()
linkObjectsIntoExecutable objFiles exeFile libOpts = do
  home <- getHomeDirectory
  let args = linkArgs home objFiles exeFile libOpts
  runCommand "gcc" args "Object to executable compilation failed with exit code: "

linkArgs :: FilePath -> [FilePath] -> FilePath -> LibraryOptions -> [String]
linkArgs home objFiles exeFile libOpts =
  ["-no-pie"] 
  ++ objFiles 
  ++ ["-o", exeFile] 
  ++ libPathArgs home libOpts
  ++ libNameArgs libOpts
  ++ [rpathArg home]

libPathArgs :: FilePath -> LibraryOptions -> [String]
libPathArgs home libOpts = 
  map ("-L" <>) (defaultLibPaths home ++ libPaths libOpts)

libNameArgs :: LibraryOptions -> [String]
libNameArgs = map ("-l" <>) . libNames

runCommand :: String -> [String] -> String -> IO ()
runCommand cmd args errMsg = do
  exitCode <- rawSystem cmd args
  case exitCode of
    ExitSuccess      -> return ()
    ExitFailure code -> logError $ errMsg <> show code

optimizeIR :: IRProgram -> Either String IRProgram
optimizeIR = Right . fixpoint runIROptimizer

checkSemantics :: Program -> Either String (Program, FuncStack)
checkSemantics = verifVars

safeRead :: FilePath -> IO (Either String String)
safeRead fp = (try (readFile fp) :: IO (Either IOException String)) <&> first (("Failed to read input file: " <>) . show)

parseLexer :: (FilePath, String) -> Either String (FilePath, [Token])
parseLexer (fp, content) = either (Left . errorBundlePretty) (Right . (fp,)) (lexer fp content)

parseAST :: (FilePath, [Token]) -> Either String Program
parseAST (fp, tokens) = parseRune fp tokens

objectFileName :: FilePath -> FilePath
objectFileName = (<> ".o") . dropExtension

isPIC :: LibraryOptions -> Bool
isPIC libOpts = libShared libOpts || libStatic libOpts

selectEmitter :: LibraryOptions -> IRProgram -> String
selectEmitter libOpts = if isPIC libOpts then emitAssemblyLib else emitAssembly

partitionByExt :: String -> [FilePath] -> ([FilePath], [FilePath])
partitionByExt ext = partition ((== ext) . takeExtension)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([], [])
  where
    go (Left e)  (ls, rs) = (e : ls, rs)
    go (Right r) (ls, rs) = (ls, r : rs)

logErrorNoExit :: String -> IO ()
logErrorNoExit msg = hPutStrLn stderr $ "\x1b[31m[ERROR]: \x1b[0m" ++ msg
