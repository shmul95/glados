import Test.Tasty

import ASTSpec (astTests)
import ParserSpec (parserTests)
import SExprSpec (sexprTests)

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ astTests,
    parserTests,
    sexprTests
  ]
