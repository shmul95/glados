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
    CompileMode (..),
    LibraryOptions (..)
  )
import System.FilePath (takeExtension, dropExtension)

data Action
  = ShowUsage
  | CompileAll        FilePath (Maybe FilePath) LibraryOptions
  | CompileAllMany    [FilePath] (Maybe FilePath) LibraryOptions
  | CompileObjToExec  FilePath (Maybe FilePath) LibraryOptions
  | CompileToObj      FilePath (Maybe FilePath)
  | CreateAsm         FilePath (Maybe FilePath)
  | Interpret         FilePath
  deriving (Show, Eq)

data CompileRule
  = All
  | ToObj
  | ToAsm
  | ToSharedLib
  | ToStaticLib
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
      "  -S                    Compile to assembly code",
      "  -shared               Build a shared library (.so)",
      "  -static-lib           Build a static library (.a)",
      "  -L<path>              Add library search path",
      "  -l<name>              Link with library"
    ]

parseArgs :: [String] -> Either String Action
parseArgs [] = Left "No command provided. Use 'rune help'."
parseArgs (cmd : rest) = parseCommand cmd rest

runCLI :: Action -> IO ()
runCLI ShowUsage = putStr usage
runCLI (Interpret inFile) = interpretPipeline inFile
runCLI (CompileAll inFile maybeOutFile libOpts) =
  let outFile = fromMaybe (defaultOutput libOpts "a.out") maybeOutFile
   in compilePipeline inFile outFile (FullCompile libOpts)
runCLI (CompileAllMany inFiles maybeOutFile libOpts) =
  let outFile = fromMaybe (defaultOutput libOpts "a.out") maybeOutFile
   in compileMultiplePipeline inFiles outFile libOpts
runCLI (CompileToObj inFile maybeOutFile) =
  let outFile = fromMaybe (dropExtension inFile ++ ".o") maybeOutFile
  in compilePipeline inFile outFile ToObject
runCLI (CreateAsm inFile maybeOutFile) =
  let outFile = fromMaybe (dropExtension inFile ++ ".asm") maybeOutFile
  in compilePipeline inFile outFile ToAssembly
runCLI (CompileObjToExec inFile maybeOutFile libOpts) =
  let outFile = fromMaybe (defaultOutput libOpts "a.out") maybeOutFile
  in compilePipeline inFile outFile (ToExecutable libOpts)

defaultOutput :: LibraryOptions -> String -> String
defaultOutput (LibraryOptions True _ _ _) _ = "libout.so"
defaultOutput (LibraryOptions _ True _ _) _ = "libout.a"
defaultOutput _ def = def

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
  (libOpts, args3)  <- parseLibraryOptions args2
  parseByRule rule outFile libOpts args3


parseByRule :: CompileRule -> Maybe FilePath -> LibraryOptions -> [String] -> Either String Action
parseByRule All outFile libOpts args = do
  (inFiles, rest) <- findInputFiles args All
  validateNoExtraArgs rest
  pure $ case inFiles of
    [single] -> isSourceFile single outFile libOpts
    _        -> CompileAllMany inFiles outFile libOpts

parseByRule ToSharedLib outFile _ args = do
  let libOpts = LibraryOptions True False [] []
  (inFiles, rest) <- findInputFiles args ToSharedLib
  validateNoExtraArgs rest
  pure $ case inFiles of
    [single] -> CompileAll single outFile libOpts
    _        -> CompileAllMany inFiles outFile libOpts

parseByRule ToStaticLib outFile _ args = do
  let libOpts = LibraryOptions False True [] []
  (inFiles, rest) <- findInputFiles args ToStaticLib
  validateNoExtraArgs rest
  pure $ case inFiles of
    [single] -> CompileAll single outFile libOpts
    _        -> CompileAllMany inFiles outFile libOpts

parseByRule rule outFile _ args = do
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
isValidInputFile ToSharedLib file = takeExtension file `elem` [".ru", ".o"]
isValidInputFile ToStaticLib file = takeExtension file `elem` [".ru", ".o"]

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

isSourceFile :: FilePath -> Maybe FilePath -> LibraryOptions -> Action
isSourceFile inFile outFile libOpts =
  case takeExtension inFile of
    ".ru" -> CompileAll inFile outFile libOpts
    _     -> CompileObjToExec inFile outFile libOpts

determineCompileRule :: [String] -> Either String (CompileRule, [String])
determineCompileRule args =
  case foldl processArg (False, False, False, False, []) args of
    (True, True, _, _, _) -> Left "Cannot use both -c and -S options together."
    (_, _, True, True, _) -> Left "Cannot use both -shared and -static-lib options together."
    (True, _, True, _, _) -> Left "Cannot use -c with -shared."
    (True, _, _, True, _) -> Left "Cannot use -c with -static-lib."
    (_, True, True, _, _) -> Left "Cannot use -S with -shared."
    (_, True, _, True, _) -> Left "Cannot use -S with -static-lib."
    (True, False, _, _, filtered) -> Right (ToObj, reverse filtered)
    (False, True, _, _, filtered) -> Right (ToAsm, reverse filtered)
    (_, _, True, _, filtered) -> Right (ToSharedLib, reverse filtered)
    (_, _, _, True, filtered) -> Right (ToStaticLib, reverse filtered)
    (False, False, False, False, filtered) -> Right (All, reverse filtered)
  where
    processArg (c, s, sh, st, acc) arg
      | arg == "-c" = (True, s, sh, st, acc)
      | arg == "-S" = (c, True, sh, st, acc)
      | arg == "-shared" = (c, s, True, st, acc)
      | arg == "-static-lib" = (c, s, sh, True, acc)
      | otherwise = (c, s, sh, st, arg : acc)

parseLibraryOptions :: [String] -> Either String (LibraryOptions, [String])
parseLibraryOptions args =
  let (paths, rest1) = extractPrefixed "-L" args
      (names, rest2) = extractPrefixed "-l" rest1
   in Right (LibraryOptions False False paths names, rest2)
  where
    extractPrefixed prefix xs =
      let (matching, others) = partition (prefix `isPrefixOf`) xs
          values = map (drop (length prefix)) matching
       in (values, others)

isPrefixOf :: String -> String -> Bool
isPrefixOf pre str = take (length pre) str == pre
