import CLISpec (cliTests)
import LexerSpec (lexerTests)
import LoggerSpec (loggerTests)
import PipelinesSpec (pipelinesTests)
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ cliTests,
        lexerTests,
        pipelinesTests,
        loggerTests
      ]
