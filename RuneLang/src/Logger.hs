module Logger
  ( logError,
    logInfo,
  )
where

import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

logError :: String -> IO ()
logError msg = do
  let red = "\x1b[31m"
  let reset = "\x1b[0m"
  hPutStrLn stderr $ red ++ "[ERROR]: " ++ reset ++ msg
  exitWith (ExitFailure 84)

logInfo :: String -> IO ()
logInfo msg = do
  let green = "\x1b[32m"
  let reset = "\x1b[0m"
  putStrLn $ green ++ "[INFO]: " ++ reset ++ msg
