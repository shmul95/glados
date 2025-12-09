import Test.Tasty

import AST.ASTSpec (astTests)
import AST.SExprToASTSpec (sexprToASTTests)
import AST.ASTErrorSpec (astErrorTests)
import ParserSpec (parserTests)
import SExprSpec (sexprTests)

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ astTests
  , sexprToASTTests
  , astErrorTests
  , parserTests
  , sexprTests
  ]
