{-
-- EPITECH PROJECT, 2025
-- Main.hs
-- File description:
-- Main.hs
-}

module Main (main) where

import CLI (parseArgs, runCLI)
import Logger (logError)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= either logError runCLI . parseArgs
