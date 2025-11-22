import Test.Tasty
import Test.Tasty.Hspec

import qualified SExprSpec
import qualified ParserSpec
import qualified ASTSpec

main :: IO ()
main = do
  sexprTests <- testSpec "SExpr" SExprSpec.spec
  parserTests <- testSpec "Parser" ParserSpec.spec
  astTests <- testSpec "AST" ASTSpec.spec
  defaultMain $ testGroup "GLaDOS Tests" [
    sexprTests,
    parserTests,
    astTests
    ]
