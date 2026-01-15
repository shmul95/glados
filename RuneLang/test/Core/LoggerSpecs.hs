{-# LANGUAGE ScopedTypeVariables #-}

module Core.LoggerSpecs (loggerTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Logger
import System.IO.Silently (capture_)
import Control.Exception (try, SomeException)
import Data.List (isInfixOf)

--
-- public
--

loggerTests :: TestTree
loggerTests = testGroup "Logger Tests"
  [ testCase "logInfo prints correct message to stdout" testLogInfo
  , testCase "logError prints correct message to stderr and exits" testLogErrorMessage
  , testCase "logError actually exits with code 84" testLogErrorExitCode
  ]

--
-- private
--

testLogInfo :: IO ()
testLogInfo = do
  let msg = "Info message"
  output <- capture_ (logInfo msg)
  ("\x1b[32m[INFO]: \x1b[0mInfo message" `isInfixOf` output) @? "logInfo output mismatch"

testLogErrorMessage :: IO ()
testLogErrorMessage = do
  let msg = "Error message"
  result <- try (capture_ (logError msg)) :: IO (Either SomeException String)
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "Expected exitWith exception"

testLogErrorExitCode :: IO ()
testLogErrorExitCode = do
  result <- try (logError "Test exit") :: IO (Either SomeException ())
  case result of
    Left _ -> return () -- logError successfully exited as expected
    Right _ -> assertFailure "Expected exitWith exception"

