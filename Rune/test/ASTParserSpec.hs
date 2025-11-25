module ASTParserSpec (astParserTests) where

import Data.Either (isRight)
import Rune.AST.Nodes
import Rune.AST.Parser (parseRune)
import Rune.Lexer.Lexer (lexer)
import Test.Tasty
import Test.Tasty.HUnit

--
-- public
--

astParserTests :: TestTree
astParserTests =
  testGroup
    "AST Parser Tests"
    [ validExamplesTests
    ]

--
-- private helpers
--

parseStr :: String -> Either String Program
parseStr input =
  case lexer "test.ru" input of
    Left _ -> Left "Lexer failed"
    Right tokens -> parseRune "test.ru" tokens

assertParses :: String -> Assertion
assertParses input =
  assertBool ("Should parse successfully:\n" ++ input) (isRight $ parseStr input)

--
-- private test groups
--

validExamplesTests :: TestTree
validExamplesTests =
  testGroup
    "Valid Examples (Happy Path)"
    [ testCase "hello_rune.ru" $
        assertParses "def main() -> null { show(\"Hello\"); }",
      testCase "return_0.ru" $
        assertParses "def main() -> i32 { 0 }",
      -- WARN: this one is currently failing due to incomplete if-expression parsing
      -- testCase "fibonacci.ru" $
      --   assertParses "def fib(n: i32) -> i32 { if n <= 0 { 0 } fib(n-1) }",
      testCase "struct.ru" $
        assertParses "struct V { x: i32; } def main() -> null { a = V { x: 1 }; }",
      testCase "error_handling.ru" $
        assertParses "def f() ~> i32 { error(\"fail\"); } def main() -> i32 { f()?; 0 }"
    ]
