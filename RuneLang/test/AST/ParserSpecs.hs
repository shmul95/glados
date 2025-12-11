module AST.ParserSpecs (parserTests) where

import Test.Tasty (TestTree, testGroup)
import AST.Parser.ParseExpressionSpecs (parseExpressionTests)
import AST.Parser.ParseBlockSpecs (parseBlockTests)
import AST.Parser.ParseTopLevelSpecs (parseTopLevelTests)
import AST.Parser.ParseTypesSpecs (parseTypesTests)

--
-- public
--

parserTests :: TestTree
parserTests =
  testGroup
    "AST Parser Tests"
    [ parseExpressionTests
    , parseBlockTests
    , parseTopLevelTests
    , parseTypesTests
    ]
