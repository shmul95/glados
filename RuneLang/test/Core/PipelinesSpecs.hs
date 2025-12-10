module Core.PipelinesSpecs (pipelinesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.IO (hClose)
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException, catch)
import Control.Monad (unless)
import Data.Char (isSpace)

import Rune.Pipelines (compilePipeline, interpretPipeline)

--
-- public
--

pipelinesTests :: TestTree
pipelinesTests =
  testGroup
    "Pipeline Tests"
    [ pipelinesValidTests
    , pipelinesInvalidTests
    ]

--
-- helpers
--

validRuneCode :: String
validRuneCode =
    "def main() -> i32 { return 42; }"

invalidRuneCode :: String
invalidRuneCode =
    "def main() -> i32 { return 42"

catchExitCode :: IO () -> IO ExitCode
catchExitCode action = catch (action >> return ExitSuccess) (\e -> return e)

assertCompileSuccess :: String -> IO ()
assertCompileSuccess code =
    withSystemTempFile "test_in.rune" (\inFile hIn ->
    withSystemTempFile "test_out.asm" $ \outFile hOut -> do
        hClose hIn
        hClose hOut
        writeFile inFile code

        result <- try $ compilePipeline inFile outFile :: IO (Either SomeException ())
        case result of
            Right _ -> do
                content <- readFile outFile
                let cleaned = take 100 $ dropWhile isSpace content
                unless (not (null content) && "section .text" `elem` lines content) $
                    assertFailure $ "Output file content is invalid/empty:\n" ++ cleaned
            Left e -> assertFailure $ "compilePipeline failed with unexpected exception: " ++ show e)

assertInterpretSuccess :: String -> IO ()
assertInterpretSuccess code =
    withSystemTempFile "test_in.rune" $ \inFile hIn -> do
        hClose hIn
        writeFile inFile code

        result <- try $ interpretPipeline inFile :: IO (Either SomeException ())
        case result of
            Right _ -> pure ()
            Left e -> assertFailure $ "interpretPipeline failed with unexpected exception: " ++ show e

assertFailureFileNotFound :: (FilePath -> IO ()) -> IO ()
assertFailureFileNotFound pipelineFunc =
    withSystemTempDirectory "temp_dir" $ \dir -> do
        let nonExistentFile = dir ++ "/non_existent.rune"
        exitCode <- catchExitCode (pipelineFunc nonExistentFile)
        exitCode @?= ExitFailure 84

assertFailurePipelineError :: (FilePath -> IO ()) -> String -> IO ()
assertFailurePipelineError pipelineFunc code =
    withSystemTempFile "test_in_err.rune" $ \inFile hIn -> do
        hClose hIn
        writeFile inFile code
        exitCode <- catchExitCode (pipelineFunc inFile)
        exitCode @?= ExitFailure 84

--
-- private
--

pipelinesValidTests :: TestTree
pipelinesValidTests =
  testGroup
    "Valid Pipeline Tests"
    [ testCase "compilePipeline success: Should create non-empty assembly file" $
        assertCompileSuccess validRuneCode
    , testCase "interpretPipeline success: Should execute without exception" $
        assertInterpretSuccess validRuneCode
    ]

pipelinesInvalidTests :: TestTree
pipelinesInvalidTests =
  testGroup
    "Invalid Pipeline Tests"
    [ testCase "compilePipeline failure: File not found (safeRead failure) should exit 84" $
        assertFailureFileNotFound (\f -> compilePipeline f "out.asm")
    , testCase "interpretPipeline failure: File not found (safeRead failure) should exit 84" $
        assertFailureFileNotFound interpretPipeline
    , testCase "compilePipeline failure: Pipeline error (Lexer/Parser) should exit 84" $
        assertFailurePipelineError (\f -> compilePipeline f "out.asm") invalidRuneCode
    , testCase "interpretPipeline failure: Pipeline error (Lexer/Parser) should exit 84" $
        assertFailurePipelineError interpretPipeline invalidRuneCode
    ]
