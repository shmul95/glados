module LoggerSpec (loggerTests) where

import Control.Exception (try)
import Data.List (isInfixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Logger (logError, logInfo)
import System.Exit (ExitCode (ExitFailure))
import System.IO
  ( BufferMode (NoBuffering),
    hClose,
    hFlush,
    hSetBuffering,
    stderr,
    stdout,
  )
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

--
-- public
--

loggerTests :: TestTree
loggerTests =
  testGroup
    "Logger Tests"
    [ testLoggerInfo,
      testLoggerError
    ]

--
-- private
--

testLoggerInfo :: TestTree
testLoggerInfo = testCase "logInfo writes colored message to stdout" $ do
  withSystemTempFile "stdout_capture" $ \path h -> do
    hSetBuffering h NoBuffering
    origOut <- hDuplicate stdout
    hDuplicateTo h stdout

    logInfo "hello"

    hFlush h
    hDuplicateTo origOut stdout
    hClose origOut
    hClose h

    contents <- readFile path
    let green = "\x1b[32m"
    let reset = "\x1b[0m"
    let expected = green ++ "[INFO]: " ++ reset ++ "hello"
    assertBool "stdout should contain colored message" (expected `isInfixOf` contents)

testLoggerError :: TestTree
testLoggerError = testCase "logError writes colored message to stderr and exits with code 84" $ do
  withSystemTempFile "stderr_capture" $ \path h -> do
    hSetBuffering h NoBuffering
    origErr <- hDuplicate stderr
    hDuplicateTo h stderr
    result <- try (logError "panic") :: IO (Either ExitCode ())
    hDuplicateTo origErr stderr
    hClose origErr
    hClose h
    contents <- readFile path
    let red = "\x1b[31m"
    let reset = "\x1b[0m"
    let expected = red ++ "[ERROR]: " ++ reset ++ "panic"
    assertBool "stderr should contain colored message" (expected `isInfixOf` contents)
    case result of
      Left (ExitFailure 84) -> return ()
      _ -> error "logError did not exit with code 84"
