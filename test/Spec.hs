module Main (main) where

import ASTParserSpec (astParserTests)
import CLISpec (cliTests)
import LexerSpec (lexerTests)
import PipelinesSpec (pipelinesTests)
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Rune Test Suites"
      [ lexerTests,
        pipelinesTests,
        cliTests,
        astParserTests
      ]
