module Main (main) where

import CLI (parseArgs, runCLI)
import Logger (logError)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= either logError runCLI . parseArgs
