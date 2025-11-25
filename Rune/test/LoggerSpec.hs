module LoggerSpec (loggerTests) where

import Control.Exception (finally, try)
import Data.List (isInfixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Logger (logError)
import System.Exit (ExitCode (ExitFailure))
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    hClose,
    hFlush,
    hSetBuffering,
    stderr,
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
    [ testLoggerError
    ]

--
-- helpers
--

captureHandle :: Handle -> IO r -> IO (String, r)
captureHandle handle action =
  withSystemTempFile "capture" $ \path h -> do
    hSetBuffering h NoBuffering
    orig <- hDuplicate handle
    hDuplicateTo h handle
    hSetBuffering handle NoBuffering
    result <- action `finally` (hFlush h >> hDuplicateTo orig handle >> hClose orig >> hClose h)
    contents <- readFile path
    return (contents, result)

--
-- private tests
--

testLoggerError :: TestTree
testLoggerError = testCase "logError writes colored message to stderr and exits with code 84" $ do
  (contents, result) <- captureHandle stderr (try $ logError "panic" :: IO (Either ExitCode ()))
  let red = "\x1b[31m"
  let reset = "\x1b[0m"
  let expected = red ++ "[ERROR]: " ++ reset ++ "panic"
  assertBool "stderr should contain colored message" (expected `isInfixOf` contents)
  case result of
    Left (ExitFailure 84) -> return ()
    _ -> error "logError did not exit with code 84"
