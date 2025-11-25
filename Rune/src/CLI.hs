module CLI
  ( Action (..),
    runCLI,
    parseArgs,
  )
where

import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import Rune.Pipelines (compilePipeline, interpretPipeline)

data Action
  = ShowUsage
  | Compile FilePath (Maybe FilePath)
  | Interpret FilePath
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
runCLI ShowUsage = putStrLn usage >> exitSuccess
runCLI (Interpret inFile) = interpretPipeline inFile
runCLI (Compile inFile maybeOutFile) =
  let outFile = fromMaybe "out" maybeOutFile
  in compilePipeline inFile outFile

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
parseBuild [] = Left "The 'build' command requires an input file."
parseBuild [file] = Right (Compile file Nothing)
parseBuild [file, "-o", outFile] = Right (Compile file (Just outFile))
parseBuild [file, "--output", outFile] = Right (Compile file (Just outFile))
parseBuild xs = Left $ "Invalid arguments for build command: " ++ unwords xs
