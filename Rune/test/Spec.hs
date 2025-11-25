import Test.Tasty

import CLISpec (cliTests)
import LexerSpec (lexerTests)
import PipelinesSpec (pipelinesTests)

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [cliTests
  , lexerTests
  , pipelinesTests
  ]
