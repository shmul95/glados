import Test.Tasty

import ASTSpec (astTests)
import ParserSpec (parserTests)
import SExprSpec (sexprTests)
-- import CLISpec (cliTests)
-- import LexerSpec (lexerTests)
-- import PipelinesSpec (pipelinesTests)

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ astTests,
    parserTests,
    sexprTests
  -- , cliTests
  -- , lexerTests
  -- , pipelinesTests
  ]
