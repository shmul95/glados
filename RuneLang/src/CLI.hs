{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module CLI
  ( Action (..),
    runCLI,
    parseArgs,
    CompileRule (..),
    determineCompileRule,
    findInputFile,
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
import Rune.Pipelines
  ( compilePipeline,
    interpretPipeline,
    CompileMode (..)
  )
import System.FilePath (takeExtension, dropExtension)

data Action
  = ShowUsage
  | CompileAll        FilePath (Maybe FilePath)
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
    [ "Usage: glados <command> [file] [options]",
      "",
      "Commands:",
      "  help           Show this help message",
      "  build [file]   Compile the given source file",
      "  run   [file]   Interpret the given source file",
      "",
      "Options:",
      "  -o, --output <file>   Specify the output file for compilation",
      "  -c                    Compile to object file",
      "  -S                    Compile to assembly code"
    ]

parseArgs :: [String] -> Either String Action
parseArgs [] = Left "No command provided. Use 'glados help'."
parseArgs (cmd : rest) = parseCommand cmd rest

runCLI :: Action -> IO ()
runCLI ShowUsage = putStr usage
runCLI (Interpret inFile) = interpretPipeline inFile
runCLI (CompileAll inFile maybeOutFile) =
  let outFile = fromMaybe "a.out" maybeOutFile
   in compilePipeline inFile outFile FullCompile
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
parseCommand cmd _ = Left $ "Invalid command: " ++ cmd ++ ". Use 'glados help'."

parseRun :: [String] -> Either String Action
parseRun [file] = Right (Interpret file)
parseRun [] = Left "The 'run' command requires an input file."
parseRun _ = Left "The 'run' command takes exactly one file argument."

parseBuild :: [String] -> Either String Action
parseBuild args = do
  (rule, args1) <- determineCompileRule args
  (inFile, args2) <- findInputFile args1 rule
  (outFile, args3) <- findOutputFile args2
  if null args3
    then pure $ case rule of
      All -> isSourceFile inFile outFile
      ToObj -> CompileToObj inFile outFile
      ToAsm -> CreateAsm inFile outFile
    else Left $ "Invalid arguments for build command: " ++ unwords args3

---
--- private methods to parse build command
---

findInputFile :: [String] -> CompileRule -> Either String (FilePath, [String])
findInputFile args rule =
  case break (isValidInputFile rule) args of
    (_, []) -> Left "No input file provided."
    (seen, file:rest) -> Right (file, seen ++ rest)

isValidInputFile :: CompileRule -> FilePath -> Bool
isValidInputFile All file = takeExtension file `elem` [".ru", ".o"]
isValidInputFile ToObj file = takeExtension file `elem` [".ru", ".asm"]
isValidInputFile ToAsm file = takeExtension file == ".ru"

findOutputFile :: [String] -> Either String (Maybe FilePath, [String])
findOutputFile [] = Right (Nothing, [])
findOutputFile args =
  case break (\x -> x `elem` ["-o", "--output"]) args of
    (before, []) -> Right (Nothing, before)
    (_, "-o":[]) -> Left "-o flag requires an output file."
    (before, "-o":file:after) -> Right (Just file, before ++ after)
    (_, "--output":[]) -> Left "--output flag requires an output file."
    (before, "--output":file:after) -> Right (Just file, before ++ after)
    (before, _:after) -> findOutputFile (before ++ after)

isSourceFile :: FilePath -> Maybe FilePath -> Action
isSourceFile inFile outFile = case takeExtension inFile == ".ru" of
  True -> CompileAll inFile outFile
  False -> CompileObjToExec inFile outFile

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
