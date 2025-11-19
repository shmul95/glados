module Main (main) where
import LexerSpec (lexerTests)

import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Rune Test Suites"
      [ lexerTests
      ]
