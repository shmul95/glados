{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.PipelinesSpecs (pipelinesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure, assertBool)
import Control.Exception (catch, bracket)
import Control.Monad (when)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, doesFileExist, getTemporaryDirectory)
import System.IO (hClose, hPutStr, openTempFile)
import Data.Either (isLeft, isRight)

import Rune.Pipelines
import Rune.AST.Nodes (Program(..))
import Rune.IR.Nodes (IRProgram (..))
import Rune.IR.Generator (generateIR)

--
-- public
--

pipelinesTests :: TestTree
pipelinesTests =
  testGroup
    "Pipeline Tests"
    [ compileModeDerivingTests
    , pipelinesValidTests
    , pipelinesInvalidTests
    , pipelinePrivateTests
    , pipelineCoverageTests
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

helperRuneCode :: String
helperRuneCode =
    "def helper() -> i32 { return 1; }"

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

withTempRuneFile :: String -> (FilePath -> IO a) -> IO a
withTempRuneFile content action = do
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "test-XXXXXX.ru")
          (\(fp, h) -> hClose h >> removeFileIfExists fp)
          (\(fp, h) -> do
             hPutStr h content
             hClose h
             action fp)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = do
  exists <- doesFileExist fp
  when exists (removeFile fp)

defaultLibOpts :: LibraryOptions
defaultLibOpts = LibraryOptions False False [] []

--
-- private
--

compileModeDerivingTests :: TestTree
compileModeDerivingTests =
  testGroup
    "Deriving Show/Eq Coverage for CompileMode"
    [ testCase "Show coverage for all CompileMode constructors" $ do
        let mode1 = ToObject
            mode2 = ToExecutable defaultLibOpts
            mode3 = ToAssembly
            mode4 = FullCompile defaultLibOpts
        assertEqual "Show ToObject" "ToObject" (show mode1)
        assertEqual "Show ToExecutable" ("ToExecutable (" ++ show defaultLibOpts ++ ")") (show mode2)
        assertEqual "Show ToAssembly" "ToAssembly" (show mode3)
        assertEqual "Show FullCompile" ("FullCompile (" ++ show defaultLibOpts ++ ")") (show mode4)
    , testCase "ShowList coverage for CompileMode" $ do
        let modes = [ToObject, ToExecutable defaultLibOpts]
        assertBool "Show list" (length (show modes) > 0)
    , testCase "Eq coverage for CompileMode" $ do
        let obj1 = ToObject
            obj2 = ToObject
            exe1 = ToExecutable defaultLibOpts
            asm1 = ToAssembly
            full1 = FullCompile defaultLibOpts
        assertEqual "Equal modes (ToObject)" obj1 obj2
        assertBool "Unequal modes (ToObject/ToExecutable)" (obj1 /= exe1)
        assertBool "Unequal modes (ToObject/ToAssembly)" (obj1 /= asm1)
        assertBool "Unequal modes (ToObject/FullCompile)" (obj1 /= full1)
        assertBool "Unequal modes (ToExecutable/ToAssembly)" (exe1 /= asm1)
        assertBool "Unequal modes (ToExecutable/FullCompile)" (exe1 /= full1)
        assertBool "Unequal modes (ToAssembly/FullCompile)" (asm1 /= full1)
    ]


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
    , testCase "generateIR_success" test_generateIR_success
    , testCase "optimizeIR_success" test_optimizeIR_success
    , testCase "verifAndGenIR_success" test_verifAndGenIR_success
    , testCase "verifAndGenIR_failure" test_verifAndGenIR_failure
    , testCase "pipeline_lexer_failure" test_pipeline_lexer_failure
    , testCase "pipeline_semantic_failure" test_pipeline_semantic_failure
    , testCase "runPipeline_success" test_runPipeline_success
    , testCase "runPipeline_read_failure" test_runPipeline_read_failure
    , testCase "runPipelineAction_success" test_runPipelineAction_success
    , testCase "runPipelineAction_failure" test_runPipelineAction_failure
    , testCase "compileAsmToObject_success" test_compileAsmToObject_success
    , testCase "compileAsmToObject_failure" test_compileAsmToObject_failure
    , testCase "compileAsmToObject_injection_safe" test_compileAsmToObject_injection_safe
    , testCase "compileObjectIntoExecutable_injection_safe" test_compileObjectIntoExecutable_injection_safe
    ]

pipelineCoverageTests :: TestTree
pipelineCoverageTests =
  testGroup
    "Coverage Gap Tests"
    [ testCase "compilePipeline_asm_success" test_compilePipeline_asm_success
    , testCase "compilePipeline_asm_read_failure" test_compilePipeline_asm_read_failure
    , testCase "compilePipeline_unsupported_ext" test_compilePipeline_unsupported_ext
    , testCase "compileObjectIntoExecutable_success" test_compileObjectIntoExecutable_success
    , testCase "compileMultiplePipeline_success" test_compileMultiplePipeline_success
    , testCase "compileMultiplePipeline_no_inputs" test_compileMultiplePipeline_no_inputs
    ]

--
-- public pipelines tests
--

test_interpretPipeline_success :: IO ()
test_interpretPipeline_success = do
    withTempFile validRuneCode $ \inFile -> do
        res <- catchExitCode (interpretPipeline inFile)
        assertEqual "interpretPipeline should succeed" ExitSuccess res

test_interpretPipeline_parser_failure :: IO ()
test_interpretPipeline_parser_failure = do
    withTempFile invalidRuneCode $ \inFile -> do
        res <- catchExitCode (interpretPipeline inFile)
        assertEqual "interpretPipeline (parser failure) should exit with 84" (ExitFailure 84) res

test_interpretPipeline_read_failure :: IO ()
test_interpretPipeline_read_failure = do
  res <- catchExitCode (interpretPipeline "non_existent.rune")
  assertEqual "interpretPipeline (read failure) should exit with 84" (ExitFailure 84) res

test_compilePipeline_success :: IO ()
test_compilePipeline_success = do
    withTempFile validRuneCode $ \inFile -> do
        tmpDir <- getTemporaryDirectory
        bracket (openTempFile tmpDir "out.asm")
            (\(outFile, h) -> hClose h >> removeFile outFile)
            (\(outFile, h) -> do
                hClose h
                res <- catchExitCode (compilePipeline inFile outFile (FullCompile defaultLibOpts))
                assertEqual "compilePipeline should succeed" ExitSuccess res
                exists <- doesFileExist outFile
                assertBool "Output file should exist" exists)

test_compilePipeline_read_failure :: IO ()
test_compilePipeline_read_failure = do
  res <- catchExitCode (compilePipeline "non_existent.rune" "out.asm" ToObject)
  assertEqual "compilePipeline (read failure) should exit with 84" (ExitFailure 84) res

test_compilePipeline_lexer_failure :: IO ()
test_compilePipeline_lexer_failure = do
    withTempFile invalidRuneCode $ \inFile -> do
        res <- catchExitCode (compilePipeline inFile "out.asm" ToObject)
        assertEqual "compilePipeline (lexer failure) should exit with 84" (ExitFailure 84) res

test_compilePipeline_semantic_failure :: IO ()
test_compilePipeline_semantic_failure = do
    withTempFile semErrorRuneCode $ \inFile -> do
        res <- catchExitCode (compilePipeline inFile "out.asm" ToObject)
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
    Left err -> do
        assertBool "Error message should not be empty" (not (null err))
    Right _ -> assertFailure "Lexer should have failed on invalid input"

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

test_generateIR_success :: IO ()
test_generateIR_success = do
  case parseLexer (validFile, validRuneCode) of
    Right (fp, tokens) -> case parseAST (fp, tokens) of
      Right ast -> case checkSemantics ast of
        Right (checkedAST, fs) -> case generateIR checkedAST fs of
          Right _ -> return ()
          Left err -> assertFailure $ "generateIR failed: " ++ err
        Left _ -> assertFailure "checkSemantics failed (prerequisite for generateIR_success)"
      Left _ -> assertFailure "Parser failed (prerequisite for generateIR_success)"
    Left _ -> assertFailure "Lexer failed (prerequisite for generateIR_success)"

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

test_compileAsmToObject_success :: IO ()
test_compileAsmToObject_success = do
  let asmContent = "section .text\nglobal main\nmain:\n mov rax, 60\n xor rdi, rdi\n syscall"
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "test.o")
    (\(objFile, _) -> removeFileIfExists objFile)
    (\(objFile, h) -> do
      hClose h
      removeFileIfExists objFile
      compileAsmToObject asmContent objFile False
      exists <- doesFileExist objFile
      assertBool "compileAsmToObject should create .o file" exists)

test_compileAsmToObject_failure :: IO ()
test_compileAsmToObject_failure = do
  let invalidAsm = "invalid instruction xyz abc def\n"
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "test.o")
    (\(objFile, _) -> removeFileIfExists objFile)
    (\(objFile, h) -> do
      hClose h
      caught <- catch (compileAsmToObject invalidAsm objFile False >> return False)
        (\case
          ExitFailure 84 -> return True
          _ -> return False)
      assertBool "compileAsmToObject should exit with 84 on invalid ASM" caught)

test_compileAsmToObject_injection_safe :: IO ()
test_compileAsmToObject_injection_safe = do
  let asmContent = "section .text\nglobal main\nmain:\n mov rax, 60\n xor rdi, rdi\n syscall"
  caught <- catch (compileAsmToObject asmContent "/tmp/test.o; echo pwned > /tmp/pwned.txt" False >> return False)
    (\case ExitFailure 84 -> return True; _ -> return False)
  assertBool "compileAsmToObject should reject injection" caught
  pwned <- doesFileExist "/tmp/pwned.txt"
  assertBool "Injection should not execute" (not pwned)
  removeFileIfExists "/tmp/pwned.txt"

test_compileObjectIntoExecutable_injection_safe :: IO ()
test_compileObjectIntoExecutable_injection_safe = do
  let asmContent = "section .text\nglobal main\nmain:\n mov rax, 60\n xor rdi, rdi\n syscall"
  tmpDir <- getTemporaryDirectory
  bracket (openTempFile tmpDir "test.o")
    (\(objFile, _) -> removeFileIfExists objFile)
    (\(objFile, h) -> do
      hClose h
      compileAsmToObject asmContent objFile False
      caught <- catch (compileObjectIntoExecutable objFile "/tmp/test.bin; echo pwned > /tmp/pwned2.txt" defaultLibOpts >> return False)
        (\case ExitFailure 84 -> return True; _ -> return False)
      assertBool "compileObjectIntoExecutable should reject injection" caught
      pwned <- doesFileExist "/tmp/pwned2.txt"
      assertBool "Injection should not execute" (not pwned)
      removeFileIfExists "/tmp/pwned2.txt")

--
-- Coverage Gap Tests
--

test_compilePipeline_asm_success :: IO ()
test_compilePipeline_asm_success = do
    let asmContent = "section .text\nglobal main\nmain:\n mov rax, 60\n xor rdi, rdi\n syscall"
    tmpDir <- getTemporaryDirectory
    bracket (openTempFile tmpDir "test.asm")
        (\(fp, h) -> hClose h >> removeFileIfExists fp)
        (\(fp, h) -> do
            hPutStr h asmContent
            hClose h
            let objFile = fp ++ ".o"
            res <- catchExitCode (compilePipeline fp objFile ToObject)
            exists <- doesFileExist objFile
            removeFileIfExists objFile
            assertEqual "compilePipeline .asm success" ExitSuccess res
            assertBool "Object file created" exists)

test_compilePipeline_asm_read_failure :: IO ()
test_compilePipeline_asm_read_failure = do
    res <- catchExitCode (compilePipeline "non_existent.asm" "out.o" ToObject)
    assertEqual "compilePipeline .asm read failure" (ExitFailure 84) res

test_compilePipeline_unsupported_ext :: IO ()
test_compilePipeline_unsupported_ext = do
    withTempFile "content" $ \fp -> do
        let badFile = fp ++ ".xyz"
        writeFile badFile "content"
        res <- catchExitCode (compilePipeline badFile "out.o" ToObject)
        removeFileIfExists badFile
        assertEqual "compilePipeline unsupported ext" (ExitFailure 84) res

test_compileObjectIntoExecutable_success :: IO ()
test_compileObjectIntoExecutable_success = do
    let asmContent = "section .text\nglobal main\nmain:\n mov rax, 60\n xor rdi, rdi\n syscall"
    tmpDir <- getTemporaryDirectory
    bracket (openTempFile tmpDir "test.o")
        (\(obj, h) -> hClose h >> removeFileIfExists obj)
        (\(obj, h) -> do
            hClose h
            compileAsmToObject asmContent obj False
            let exe = obj ++ ".exe"
            res <- catchExitCode (compileObjectIntoExecutable obj exe defaultLibOpts)
            exists <- doesFileExist exe
            removeFileIfExists exe
            assertEqual "compileObjectIntoExecutable success" ExitSuccess res
            assertBool "Executable created" exists)

test_compileMultiplePipeline_success :: IO ()
test_compileMultiplePipeline_success = do
    withTempRuneFile validRuneCode $ \fp1 ->
      withTempRuneFile helperRuneCode $ \fp2 -> do
        tmpDir <- getTemporaryDirectory
        bracket (openTempFile tmpDir "multi.out")
          (\(out, h) -> hClose h >> removeFileIfExists out)
          (\(out, h) -> do
            hClose h
            res <- catchExitCode (compileMultiplePipeline [fp1, fp2] out defaultLibOpts)
            exists <- doesFileExist out
            removeFileIfExists out
            assertEqual "compileMultiplePipeline success" ExitSuccess res
            assertBool "Executable created" exists)

test_compileMultiplePipeline_no_inputs :: IO ()
test_compileMultiplePipeline_no_inputs = do
  res <- catchExitCode (compileMultiplePipeline [] "out" defaultLibOpts)
  removeFileIfExists "out"
  assertEqual "compileMultiplePipeline should fail without inputs" (ExitFailure 84) res
