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
import Control.Monad ((>=>))
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
  | CompileToObj      FilePath (Maybe FilePath) LibraryOptions
  | CreateAsm         FilePath (Maybe FilePath) LibraryOptions
  | Interpret         FilePath LibraryOptions
  deriving (Show, Eq)

data CompileRule
  = All
  | ToObj
  | ToAsm
  | ToSharedLib
  | ToStaticLib
  deriving (Show, Eq)

validExtensions :: CompileRule -> [String]
validExtensions All         = [".ru", ".rune", ".o"]
validExtensions ToObj       = [".ru", ".rune", ".asm"]
validExtensions ToAsm       = [".ru", ".rune"]
validExtensions ToSharedLib = [".ru", ".rune", ".o"]
validExtensions ToStaticLib = [".ru", ".rune", ".o"]

usage :: String
usage = unlines
  [ "Usage: rune <command> [file] [options]"
  , ""
  , "Commands:"
  , "  help           Show this help message"
  , "  build [file]   Compile the given source file (.ru, .rune, .asm)"
  , "  run   [file]   Show the IR of the given source file (.ru, .rune)"
  , ""
  , "Options:"
  , "  -o, --output <file>   Specify the output file for compilation"
  , "  -c                    Compile to object file"
  , "  -S                    Compile to assembly code"
  , "  -shared               Build a shared library (.so)"
  , "  -static-lib           Build a static library (.a)"
  , "  -L<path>              Add library search path"
  , "  -l<name>              Link with library"
  , "  -I<path>              Add include search path for 'use' statements"
  ]

parseArgs :: [String] -> Either String Action
parseArgs [] = Left "No command provided. Use 'rune help'."
parseArgs (cmd : rest) = parseCommand cmd rest

parseCommand :: String -> [String] -> Either String Action
parseCommand cmd
  | cmd `elem` ["help", "--help", "-h"]           = const $ pure ShowUsage
  | cmd `elem` ["run", "--run", "-r"]             = parseRun
  | cmd `elem` ["build", "--build", "-b"]         = parseBuild
  | otherwise = const . Left $ "Invalid command: " ++ cmd ++ ". Use 'rune help'."

parseRun :: [String] -> Either String Action
parseRun args = do
  (libOpts, rest) <- parseLibraryOptions args
  case rest of
    [file] -> Right $ Interpret file libOpts
    []     -> Left "The 'run' command requires an input file."
    _      -> Left "The 'run' command takes exactly one file argument."

parseBuild :: [String] -> Either String Action
parseBuild args = do
  (rule, args1) <- determineCompileRule args
  (outFile, args2) <- findOutputFile args1
  (libOpts, args3) <- parseLibraryOptions args2
  parseByRule rule outFile libOpts args3

parseByRule :: CompileRule -> Maybe FilePath -> LibraryOptions -> [String] -> Either String Action
parseByRule rule outFile libOpts args = 
  case rule of
    All         -> parseMultiOrSingle rule outFile libOpts args
    ToSharedLib -> parseMultiOrSingle rule outFile (setShared libOpts) args
    ToStaticLib -> parseMultiOrSingle rule outFile (setStatic libOpts) args
    ToObj       -> parseSingleFile rule outFile libOpts args CompileToObj
    ToAsm       -> parseSingleFile rule outFile libOpts args CreateAsm
  where
    setShared opts = opts { libShared = True, libStatic = False }
    setStatic opts = opts { libShared = False, libStatic = True }

parseMultiOrSingle :: CompileRule -> Maybe FilePath -> LibraryOptions -> [String] -> Either String Action
parseMultiOrSingle rule outFile libOpts args = do
  (files, rest) <- findInputFiles args rule
  validateNoExtraArgs rest
  pure $ case files of
    [single] -> isSourceFile single outFile libOpts
    multiple -> CompileAllMany multiple outFile libOpts

parseSingleFile :: CompileRule -> Maybe FilePath -> LibraryOptions -> [String] -> (FilePath -> Maybe FilePath -> LibraryOptions -> Action) -> Either String Action
parseSingleFile rule outFile libOpts args constructor = do
  (file, rest) <- findInputFile args rule
  validateNoExtraArgs rest
  pure $ constructor file outFile libOpts

runCLI :: Action -> IO ()
runCLI ShowUsage = putStr usage
runCLI (Interpret inFile libOpts) = interpretPipeline inFile libOpts
runCLI (CompileAll inFile maybeOutFile libOpts) =
  compilePipeline inFile (computeOutput libOpts maybeOutFile "a.out") (FullCompile libOpts) libOpts
runCLI (CompileAllMany inFiles maybeOutFile libOpts) =
  compileMultiplePipeline inFiles (computeOutput libOpts maybeOutFile "a.out") libOpts
runCLI (CompileToObj inFile maybeOutFile libOpts) =
  compilePipeline inFile (computeDefaultOutput inFile maybeOutFile ".o") ToObject libOpts
runCLI (CreateAsm inFile maybeOutFile libOpts) =
  compilePipeline inFile (computeDefaultOutput inFile maybeOutFile ".asm") ToAssembly libOpts
runCLI (CompileObjToExec inFile maybeOutFile libOpts) =
  compilePipeline inFile (computeOutput libOpts maybeOutFile "a.out") (ToExecutable libOpts) libOpts

computeOutput :: LibraryOptions -> Maybe FilePath -> String -> String
computeOutput libOpts maybePath def = fromMaybe (defaultOutput libOpts def) maybePath

defaultOutput :: LibraryOptions -> String -> String
defaultOutput (LibraryOptions True _ _ _ _) _ = "libout.so"
defaultOutput (LibraryOptions _ True _ _ _) _ = "libout.a"
defaultOutput _ def = def

computeDefaultOutput :: FilePath -> Maybe FilePath -> String -> String
computeDefaultOutput inFile maybeOutFile ext = 
  fromMaybe (dropExtension inFile ++ ext) maybeOutFile

isValidInputFile :: CompileRule -> FilePath -> Bool
isValidInputFile rule = (`elem` validExtensions rule) . takeExtension

isSourceFile :: FilePath -> Maybe FilePath -> LibraryOptions -> Action
isSourceFile inFile outFile libOpts
  | takeExtension inFile `elem` [".ru", ".rune"] = CompileAll inFile outFile libOpts
  | otherwise                                     = CompileObjToExec inFile outFile libOpts

findInputFile :: [String] -> CompileRule -> Either String (FilePath, [String])
findInputFile args rule = case break (isValidInputFile rule) args of
  (_, [])            -> Left "No input file provided."
  (seen, file:rest)  -> Right (file, seen ++ rest)

findInputFiles :: [String] -> CompileRule -> Either String ([FilePath], [String])
findInputFiles args rule =
  let (valids, rest) = partition (isValidInputFile rule) args
   in if null valids then Left "No input file provided." else Right (valids, rest)

findOutputFile :: [String] -> Either String (Maybe FilePath, [String])
findOutputFile args = case break (`elem` ["-o", "--output"]) args of
  (before, [])                     -> Right (Nothing, before)
  (_, ["-o"])                      -> Left "-o flag requires an output file."
  (before, "-o":file:after)        -> Right (Just file, before ++ after)
  (_, ["--output"])                -> Left "--output flag requires an output file."
  (before, "--output":file:after)  -> Right (Just file, before ++ after)
  (before, _:after)                -> findOutputFile (before ++ after)

determineCompileRule :: [String] -> Either String (CompileRule, [String])
determineCompileRule = pure . foldl processArg initialState >=> decideRule
  where
    initialState = (False, False, False, False, [])
    
    processArg (c, s, sh, st, acc) arg = case arg of
      "-c"          -> (True, s, sh, st, acc)
      "-S"          -> (c, True, sh, st, acc)
      "-shared"     -> (c, s, True, st, acc)
      "-static-lib" -> (c, s, sh, True, acc)
      _             -> (c, s, sh, st, arg : acc)
    
    decideRule (True, True, _, _, _) = Left "Cannot use both -c and -S options together."
    decideRule (_, _, True, True, _) = Left "Cannot use both -shared and -static-lib options together."
    decideRule (True, _, True, _, _) = Left "Cannot use -c with -shared."
    decideRule (True, _, _, True, _) = Left "Cannot use -c with -static-lib."
    decideRule (_, True, True, _, _) = Left "Cannot use -S with -shared."
    decideRule (_, True, _, True, _) = Left "Cannot use -S with -static-lib."
    decideRule (True, False, _, _, filtered) = Right (ToObj, reverse filtered)
    decideRule (False, True, _, _, filtered) = Right (ToAsm, reverse filtered)
    decideRule (_, _, True, _, filtered) = Right (ToSharedLib, reverse filtered)
    decideRule (_, _, _, True, filtered) = Right (ToStaticLib, reverse filtered)
    decideRule (False, False, False, False, filtered) = Right (All, reverse filtered)

parseLibraryOptions :: [String] -> Either String (LibraryOptions, [String])
parseLibraryOptions args =
  let (paths, rest1) = extractPrefixed "-L" args
      (names, rest2) = extractPrefixed "-l" rest1
      (includes, rest3) = extractPrefixed "-I" rest2
   in Right (LibraryOptions False False paths names includes, rest3)

extractPrefixed :: String -> [String] -> ([String], [String])
extractPrefixed prefix xs =
  let (matching, others) = partition (isPrefixOf prefix) xs
      values = map (drop (length prefix)) matching
   in (values, others)

isPrefixOf :: String -> String -> Bool
isPrefixOf pre str = take (length pre) str == pre

validateNoExtraArgs :: [String] -> Either String ()
validateNoExtraArgs [] = Right ()
validateNoExtraArgs args = Left $ "Invalid arguments for build command: " ++ unwords args
