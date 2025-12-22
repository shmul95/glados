{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module CLI
  ( Action (..),
    runCLI,
    parseArgs,
    CompileRule (..),
    determineCompileRule,
    findInputFile,
    findInputFiles,
    findOutputFile,
    isValidInputFile,
    isSourceFile,
  )
where
#else
module CLI
  ( Action (..),
    runCLI,
    parseArgs,
  )
where
#endif

import Data.Maybe (fromMaybe)
import Data.List (partition)
import Rune.Pipelines
  ( compilePipeline,
    compileMultiplePipeline,
    interpretPipeline,
    CompileMode (..)
  )
import System.FilePath (takeExtension, dropExtension)

data Action
  = ShowUsage
  | CompileAll        FilePath (Maybe FilePath)
  | CompileAllMany    [FilePath] (Maybe FilePath)
  | CompileObjToExec  FilePath (Maybe FilePath)
  | CompileToObj      FilePath (Maybe FilePath)
  | CreateAsm         FilePath (Maybe FilePath)
  | Interpret         FilePath
  deriving (Show, Eq)

data CompileRule
  = All
  | ToObj
  | ToAsm
  deriving (Show, Eq)

usage :: String
usage =
  unlines
    [ "Usage: rune <command> [file] [options]",
      "",
      "Commands:",
      "  help           Show this help message",
      "  build [file]   Compile the given source file",
      "  run   [file]   Show the IR of the given source file",
      "",
      "Options:",
      "  -o, --output <file>   Specify the output file for compilation",
      "  -c                    Compile to object file",
      "  -S                    Compile to assembly code"
    ]

parseArgs :: [String] -> Either String Action
parseArgs [] = Left "No command provided. Use 'rune help'."
parseArgs (cmd : rest) = parseCommand cmd rest

runCLI :: Action -> IO ()
runCLI ShowUsage = putStr usage
runCLI (Interpret inFile) = interpretPipeline inFile
runCLI (CompileAll inFile maybeOutFile) =
  let outFile = fromMaybe "a.out" maybeOutFile
   in compilePipeline inFile outFile FullCompile
runCLI (CompileAllMany inFiles maybeOutFile) =
  let outFile = fromMaybe "a.out" maybeOutFile
   in compileMultiplePipeline inFiles outFile
runCLI (CompileToObj inFile maybeOutFile) =
  let outFile = fromMaybe (dropExtension inFile ++ ".o") maybeOutFile
  in compilePipeline inFile outFile ToObject
runCLI (CreateAsm inFile maybeOutFile) =
  let outFile = fromMaybe (dropExtension inFile ++ ".asm") maybeOutFile
  in compilePipeline inFile outFile ToAssembly
runCLI (CompileObjToExec inFile maybeOutFile) =
  let outFile = fromMaybe "a.out" maybeOutFile
  in compilePipeline inFile outFile ToExecutable

parseCommand :: String -> [String] -> Either String Action
parseCommand "help" _ = pure ShowUsage
parseCommand "--help" _ = pure ShowUsage
parseCommand "-h" _ = pure ShowUsage
parseCommand "run" rest = parseRun rest
parseCommand "--run" rest = parseRun rest
parseCommand "-r" rest = parseRun rest
parseCommand "build" rest = parseBuild rest
parseCommand "--build" rest = parseBuild rest
parseCommand "-b" rest = parseBuild rest
parseCommand cmd _ = Left $ "Invalid command: " ++ cmd ++ ". Use 'rune help'."

parseRun :: [String] -> Either String Action
parseRun [file] = Right (Interpret file)
parseRun [] = Left "The 'run' command requires an input file."
parseRun _ = Left "The 'run' command takes exactly one file argument."


parseBuild :: [String] -> Either String Action
parseBuild args = do
  (rule, args1)     <- determineCompileRule args
  (outFile, args2)  <- findOutputFile args1
  parseByRule rule outFile args2


parseByRule :: CompileRule -> Maybe FilePath -> [String] -> Either String Action
parseByRule All outFile args = do
  (inFiles, rest) <- findInputFiles args All
  validateNoExtraArgs rest
  pure $ case inFiles of
    [single] -> isSourceFile single outFile
    _        -> CompileAllMany inFiles outFile

parseByRule rule outFile args = do
  (inFile, rest) <- findInputFile args rule
  validateNoExtraArgs rest
  pure $ case rule of
    ToObj -> CompileToObj inFile outFile
    ToAsm -> CreateAsm   inFile outFile


validateNoExtraArgs :: [String] -> Either String ()
validateNoExtraArgs []   = Right ()
validateNoExtraArgs args =
  Left $ "Invalid arguments for build command: " ++ unwords args



---
--- private methods to parse build command
---

findInputFile :: [String] -> CompileRule -> Either String (FilePath, [String])
findInputFile args rule =
  case break (isValidInputFile rule) args of
    (_, []) -> Left "No input file provided."
    (seen, file:rest) -> Right (file, seen ++ rest)

findInputFiles :: [String] -> CompileRule -> Either String ([FilePath], [String])
findInputFiles args rule =
  let (valids, rest) = partition (isValidInputFile rule) args
   in if null valids then Left "No input file provided." else Right (valids, rest)

isValidInputFile :: CompileRule -> FilePath -> Bool
isValidInputFile All file = takeExtension file `elem` [".ru", ".o"]
isValidInputFile ToObj file = takeExtension file `elem` [".ru", ".asm"]
isValidInputFile ToAsm file = takeExtension file == ".ru"

findOutputFile :: [String] -> Either String (Maybe FilePath, [String])
findOutputFile [] = Right (Nothing, [])
findOutputFile args =
  case break (\x -> x `elem` ["-o", "--output"]) args of
    (before, []) -> Right (Nothing, before)
    (_, ["-o"]) -> Left "-o flag requires an output file."
    (before, "-o":file:after) -> Right (Just file, before ++ after)
    (_, ["--output"]) -> Left "--output flag requires an output file."
    (before, "--output":file:after) -> Right (Just file, before ++ after)
    (before, _:after) -> findOutputFile (before ++ after)

isSourceFile :: FilePath -> Maybe FilePath -> Action
isSourceFile inFile outFile =
  case takeExtension inFile of
    ".ru" -> CompileAll inFile outFile
    _     -> CompileObjToExec inFile outFile

determineCompileRule :: [String] -> Either String (CompileRule, [String])
determineCompileRule args =
  case foldl processArg (False, False, []) args of
    (True, True, _) -> Left "Cannot use both -c and -S options together."
    (True, False, filtered) -> Right (ToObj, reverse filtered)
    (False, True, filtered) -> Right (ToAsm, reverse filtered)
    (False, False, filtered) -> Right (All, reverse filtered)
  where
    processArg (c, s, acc) arg
      | arg == "-c" = (True, s, acc)
      | arg == "-S" = (c, True, acc)
      | otherwise = (c, s, arg : acc)
