module Main (main) where

import LexerSpec (lexerTests)
import PipelinesSpec (pipelinesTests)
import CLISpec (cliTests)

import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Rune Test Suites"
      [ lexerTests
        , pipelinesTests
        , cliTests
      ]
