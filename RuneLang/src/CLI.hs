module CLI
  ( Action (..),
    runCLI,
    parseArgs,
  )
where

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
  | ToExec
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
      "  -o, --output   Specify the output file for compilation"
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

findInputFile :: [String] -> CompileRule -> Either String (FilePath, [String])
findInputFile args rule =
  case break (isValidInputFile rule) args of
    (_, []) -> Left "No input file provided."
    (seen, file:rest) -> Right (file, seen ++ rest)

isValidInputFile :: CompileRule -> FilePath -> Bool
isValidInputFile All file = takeExtension file `elem` [".ru", ".o"]
isValidInputFile ToObj file = takeExtension file `elem` [".ru", ".asm"]
isValidInputFile ToAsm file = takeExtension file == ".ru"
isValidInputFile ToExec file = takeExtension file == ".o"

findOutputFile :: [String] -> Either String (Maybe FilePath, [String])
findOutputFile [] = Right (Nothing, [])
findOutputFile args =
  case break (\x -> x `elem` ["-o", "--output"]) args of
    (before, []) -> Right (Nothing, before)
    (_, "-o":[]) -> Left "Error: -o flag requires an output file."
    (before, "-o":file:after) -> Right (Just file, before ++ after)
    (_, "--output":[]) -> Left "Error: --output flag requires an output file."
    (before, "--output":file:after) -> Right (Just file, before ++ after)
    (before, _:after) -> findOutputFile (before ++ after)

determineCompileRule :: [String] -> Either String (CompileRule, [String])
determineCompileRule args
  | hasBoth = Left "Cannot use both -c and -S options together."
  | "-c" `elem` args = Right (ToObj, filter (/= "-c") args)
  | "-S" `elem` args = Right (ToAsm, filter (/= "-S") args)
  | otherwise = Right (All, args)
  where
    hasBoth = "-c" `elem` args && "-S" `elem` args

parseBuild :: [String] -> Either String Action
parseBuild args = do
  (rule, args1) <- determineCompileRule args
  (inFile, args2) <- findInputFile args1 rule
  (outFile, args3) <- findOutputFile args2
  if null args3
    then pure $ case rule of
      All -> CompileAll inFile outFile
      ToObj -> CompileToObj inFile outFile
      ToAsm -> CreateAsm inFile outFile
      ToExec -> CompileObjToExec inFile outFile
    else Left $ "Invalid arguments for build command: " ++ unwords args3
