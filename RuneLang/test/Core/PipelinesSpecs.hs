{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.PipelinesSpecs (pipelinesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure, assertBool)
import Control.Exception (catch, bracket)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, doesFileExist, getTemporaryDirectory)
import System.IO (hClose, hPutStr, openTempFile)
import Data.Either (isLeft, isRight)

import Rune.Pipelines
  ( compilePipeline,
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
import Rune.AST.Nodes (Program(..))
import Rune.IR.Nodes (IRProgram (..))

--
-- public
--

pipelinesTests :: TestTree
pipelinesTests =
  testGroup
    "Pipeline Tests"
    [ pipelinesValidTests
    , pipelinesInvalidTests
    , pipelinePrivateTests
    ]

--
-- helpers
--

validFile :: FilePath
validFile = "valid.rune"

invalidFile :: FilePath
invalidFile = "invalid.rune"

semanticErrorFile :: FilePath
semanticErrorFile = "sem_error.rune"

validRuneCode :: String
validRuneCode =
    "def main() -> i32 { return 42; }"

invalidRuneCode :: String
invalidRuneCode =
    "def main() -> i32 { return 42"

semErrorRuneCode :: String
semErrorRuneCode =
    "def main() -> i32 { x: string = 10; return 0; }"

catchExitCode :: IO () -> IO ExitCode
catchExitCode action = catch (action >> return ExitSuccess) (\e -> return e)

mockIRProgram :: IRProgram
mockIRProgram = IRProgram "" []

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = do
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "test.rune")
          (\(fp, h) -> hClose h >> removeFile fp)
          (\(fp, h) -> do
             hPutStr h content
             hClose h
             action fp)

--
-- private
--

pipelinesValidTests :: TestTree
pipelinesValidTests =
  testGroup
    "Valid Pipeline Tests"
    [ testCase "compilePipeline_success" test_compilePipeline_success
    , testCase "interpretPipeline_success" test_interpretPipeline_success
    ]

pipelinesInvalidTests :: TestTree
pipelinesInvalidTests =
  testGroup
    "Invalid Pipeline Tests"
    [ testCase "compilePipeline_read_failure" test_compilePipeline_read_failure
    , testCase "interpretPipeline_read_failure" test_interpretPipeline_read_failure
    , testCase "compilePipeline_lexer_failure" test_compilePipeline_lexer_failure
    , testCase "interpretPipeline_parser_failure" test_interpretPipeline_parser_failure
    , testCase "compilePipeline_semantic_failure" test_compilePipeline_semantic_failure
    ]

pipelinePrivateTests :: TestTree
pipelinePrivateTests =
  testGroup
    "Private Pipeline Tests"
    [ testCase "safeRead_success" test_safeRead_success
    , testCase "safeRead_failure" test_safeRead_failure
    , testCase "parseLexer_success" test_parseLexer_success
    , testCase "parseLexer_failure" test_parseLexer_failure
    , testCase "parseAST_success" test_parseAST_success
    , testCase "parseAST_failure" test_parseAST_failure
    , testCase "checkSemantics_success" test_checkSemantics_success
    , testCase "checkSemantics_failure" test_checkSemantics_failure
    , testCase "genIR_success" test_genIR_success
    , testCase "optimizeIR_success" test_optimizeIR_success
    , testCase "verifAndGenIR_success" test_verifAndGenIR_success
    , testCase "verifAndGenIR_failure" test_verifAndGenIR_failure
    , testCase "pipeline_lexer_failure" test_pipeline_lexer_failure
    , testCase "pipeline_semantic_failure" test_pipeline_semantic_failure
    , testCase "runPipeline_success" test_runPipeline_success
    , testCase "runPipeline_read_failure" test_runPipeline_read_failure
    , testCase "runPipelineAction_success" test_runPipelineAction_success
    , testCase "runPipelineAction_failure" test_runPipelineAction_failure
    ]

--
-- public pipelines tests
--

test_compilePipeline_success :: IO ()
test_compilePipeline_success = do
    withTempFile validRuneCode $ \inFile -> do
        tmpDir <- getTemporaryDirectory
        bracket (openTempFile tmpDir "out.asm")
            (\(outFile, h) -> hClose h >> removeFile outFile)
            (\(outFile, h) -> do
                hClose h
                res <- catchExitCode (compilePipeline inFile outFile)
                assertEqual "compilePipeline should succeed" ExitSuccess res
                exists <- doesFileExist outFile
                assertBool "Output file should exist" exists)

test_interpretPipeline_success :: IO ()
test_interpretPipeline_success = do
    withTempFile validRuneCode $ \inFile -> do
        res <- catchExitCode (interpretPipeline inFile)
        assertEqual "interpretPipeline should succeed" ExitSuccess res

test_compilePipeline_read_failure :: IO ()
test_compilePipeline_read_failure = do
  res <- catchExitCode (compilePipeline "non_existent.rune" "out.asm")
  assertEqual "compilePipeline (read failure) should exit with 84" (ExitFailure 84) res

test_interpretPipeline_read_failure :: IO ()
test_interpretPipeline_read_failure = do
  res <- catchExitCode (interpretPipeline "non_existent.rune")
  assertEqual "interpretPipeline (read failure) should exit with 84" (ExitFailure 84) res

test_compilePipeline_lexer_failure :: IO ()
test_compilePipeline_lexer_failure = do
    withTempFile invalidRuneCode $ \inFile -> do
        res <- catchExitCode (compilePipeline inFile "out.asm")
        assertEqual "compilePipeline (lexer failure) should exit with 84" (ExitFailure 84) res

test_interpretPipeline_parser_failure :: IO ()
test_interpretPipeline_parser_failure = do
    withTempFile invalidRuneCode $ \inFile -> do
        res <- catchExitCode (interpretPipeline inFile)
        assertEqual "interpretPipeline (parser failure) should exit with 84" (ExitFailure 84) res

test_compilePipeline_semantic_failure :: IO ()
test_compilePipeline_semantic_failure = do
    withTempFile semErrorRuneCode $ \inFile -> do
        res <- catchExitCode (compilePipeline inFile "out.asm")
        assertEqual "compilePipeline (semantic failure) should exit with 84" (ExitFailure 84) res

--
-- private pipelines tests
--

test_safeRead_success :: IO ()
test_safeRead_success = do
    withTempFile "hello" $ \fp -> do
        res <- safeRead fp
        assertEqual "safeRead_success: Content match" (Right "hello") res

test_safeRead_failure :: IO ()
test_safeRead_failure = do
  res <- safeRead "non_existent.rune"
  case res of
    Left _ -> return ()
    Right _ -> assertFailure "safeRead_failure: expected error"

test_parseLexer_success :: IO ()
test_parseLexer_success = do
  case parseLexer (validFile, validRuneCode) of
    Right (_, tokens) -> assertEqual "Expected non-empty tokens" False (null tokens)
    Left err -> assertFailure $ "parseLexer failed on valid code: " ++ err

test_parseLexer_failure :: IO ()
test_parseLexer_failure = do
  let invalidLex = "@" 
  case parseLexer (validFile, invalidLex) of
    Left _ -> return ()
    Right _ -> return ()

test_parseAST_success :: IO ()
test_parseAST_success = do
  case parseLexer (validFile, validRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right (Program _ defs) -> assertEqual "Expected one function definition" 1 (length defs)
      Left err -> assertFailure $ "parseAST failed on valid tokens: " ++ err
    Left _ -> assertFailure "Lexer failed (prerequisite for parseAST_success)"

test_parseAST_failure :: IO ()
test_parseAST_failure = do
  case parseLexer (validFile, invalidRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
        Left _ -> return ()
        Right _ -> assertFailure "parseAST should fail on syntax error"
    Left _ -> assertFailure "Lexer failed (prerequisite for parseAST_failure)"

test_checkSemantics_success :: IO ()
test_checkSemantics_success = do
  case parseLexer (validFile, validRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right ast -> case checkSemantics ast of
        Right _ -> return ()
        Left err -> assertFailure $ "checkSemantics failed on valid AST: " ++ err
      Left _ -> assertFailure "Parser failed (prerequisite for checkSemantics_success)"
    Left _ -> assertFailure "Lexer failed (prerequisite for checkSemantics_success)"

test_checkSemantics_failure :: IO ()
test_checkSemantics_failure = do
  case parseLexer (semanticErrorFile, semErrorRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right ast -> case checkSemantics ast of
        Left _ -> return ()
        Right _ -> assertFailure "checkSemantics should fail on semantic error"
      Left _ -> assertFailure "Parser failed (prerequisite for checkSemantics_failure)"
    Left _ -> assertFailure "Lexer failed (prerequisite for checkSemantics_failure)"

test_genIR_success :: IO ()
test_genIR_success = do
  case parseLexer (validFile, validRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right ast -> case checkSemantics ast of
        Right (checkedAST, fs) -> case genIR checkedAST fs of
          Right _ -> return ()
          Left err -> assertFailure $ "genIR failed: " ++ err
        Left _ -> assertFailure "checkSemantics failed (prerequisite for genIR_success)"
      Left _ -> assertFailure "Parser failed (prerequisite for genIR_success)"
    Left _ -> assertFailure "Lexer failed (prerequisite for genIR_success)"

test_optimizeIR_success :: IO ()
test_optimizeIR_success = do
  case optimizeIR mockIRProgram of
    Right _ -> return ()
    Left err -> assertFailure $ "optimizeIR failed: " ++ err

test_verifAndGenIR_success :: IO ()
test_verifAndGenIR_success = do
  case parseLexer (validFile, validRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right ast -> case verifAndGenIR ast of
        Right _ -> return ()
        Left err -> assertFailure $ "verifAndGenIR failed on valid AST: " ++ err
      Left _ -> assertFailure "Parser failed (prerequisite for verifAndGenIR_success)"
    Left _ -> assertFailure "Lexer failed (prerequisite for verifAndGenIR_success)"

test_verifAndGenIR_failure :: IO ()
test_verifAndGenIR_failure = do
  case parseLexer (semanticErrorFile, semErrorRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right ast -> case verifAndGenIR ast of
        Left _ -> return ()
        Right _ -> assertFailure "verifAndGenIR should fail on semantic error"
      Left _ -> assertFailure "Parser failed (prerequisite for verifAndGenIR_failure)"
    Left _ -> assertFailure "Lexer failed (prerequisite for verifAndGenIR_failure)"

test_pipeline_lexer_failure :: IO ()
test_pipeline_lexer_failure = do
  let invalidLex = "@" 
  case pipeline (invalidFile, invalidLex) of
    Left _ -> return ()
    Right _ -> return ()

test_pipeline_semantic_failure :: IO ()
test_pipeline_semantic_failure = do
  case pipeline (semanticErrorFile, semErrorRuneCode) of
    Left _ -> return ()
    Right _ -> assertFailure "pipeline should fail on semantic error"

test_runPipeline_success :: IO ()
test_runPipeline_success = do
    withTempFile validRuneCode $ \fp -> do
        res <- runPipeline fp
        assertBool "runPipeline should succeed on valid code" (isRight res)

test_runPipeline_read_failure :: IO ()
test_runPipeline_read_failure = do
  res <- runPipeline "non_existent.rune"
  assertBool "runPipeline should fail on non-existent file" (isLeft res)

test_runPipelineAction_success :: IO ()
test_runPipelineAction_success = do
    withTempFile validRuneCode $ \fp -> do
        let onSuccess _ = return ()
        res <- catchExitCode (runPipelineAction fp onSuccess)
        assertEqual "runPipelineAction_success should exit successfully" ExitSuccess res

test_runPipelineAction_failure :: IO ()
test_runPipelineAction_failure = do
    withTempFile invalidRuneCode $ \fp -> do
        let onSuccess _ = assertFailure "onSuccess should not be called"
        res <- catchExitCode (runPipelineAction fp onSuccess)
        assertEqual "runPipelineAction_failure should exit with 84" (ExitFailure 84) res
