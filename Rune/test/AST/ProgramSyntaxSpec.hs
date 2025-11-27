module AST.ProgramSyntaxSpec (programSyntaxTests) where

import Rune.AST.Nodes (Program)
import Rune.AST.Parser (parseRune)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (errorBundlePretty)

--
-- helper
--
parseLexer :: (FilePath, String) -> Either String (FilePath, [Token])
parseLexer (fp, content) =
  case lexer fp content of
    Left err -> Left $ errorBundlePretty err
    Right tokens -> Right (fp, tokens)

runParse :: String -> Either String Program
runParse src = do
  (fp, tokens) <- parseLexer ("test_file.ru", src)
  parseRune fp tokens

assertValidSyntax :: String -> Assertion
assertValidSyntax source =
  case runParse source of
    Left err -> assertFailure $ "Parsing failed on valid syntax:\n" ++ source ++ "\nError: " ++ err
    Right _ -> assertBool "Parsing succeeded" True

assertInvalidSyntax :: String -> Assertion
assertInvalidSyntax source =
  case runParse source of
    Left _ -> assertBool "Parsing failed as expected" True
    Right prog -> assertFailure $ "Parsing succeeded on invalid syntax:\n" ++ source ++ "\nAST: " ++ show prog

--
-- public
--

programSyntaxTests :: TestTree
programSyntaxTests =
  testGroup
    "AST Program Syntax Tests"
    [ validProgramSyntaxTests,
      invalidProgramSyntaxTests
    ]

--
-- private test groups
--

validSyntaxExamples :: [String]
validSyntaxExamples =
  [ 
    "def main() -> null { str: string = \"Warszawa\"; num: i32 = 1; k: i32; for c in str { if c == 'a' { next; } show(c); } for i = 0 to 10 { ++i; } k = i; show(k); for j: i32 = 0 to 25 { j *= 2; } k = j; show(k); loop { if num > k { stop; } num += 3; } show(j); show(num); }",
    "def f(n: i32)->i32 { if n <= 1 { n } else { f(n - 1) + f(n - 2) } } struct Person { name: string; age: i32; def greet(self)->null{show(\"hello \");show(self.name);show(\"\n\");} } def main()->null { p = Person { name: \"Alice\", age: 30 }; p.greet(); show(\"Fibonacci(10) = \"); show(f(10)); show(\"\n\"); }",
    "def main() -> null { x = 10; }",
    "def main() -> null { y: f64; }",
    "def main() -> null { show(\"hi\") }",
    "def main() -> null { }",
    "def main() -> null { for i to 10 { show(i); } }"
  ]

invalidSyntaxExamples :: [String]
invalidSyntaxExamples =
  [ "def main() -> null { var; }",
    "def main() -> null { var = 10 }",
    "struct { }",
    "def main() -> null { : i32 = 5; }",
    "def main() -> null { stop; }",
    "def main() -> null { 1 = 2; }"
  ]

validProgramSyntaxTests :: TestTree
validProgramSyntaxTests =
  testGroup "Valid Syntax" $
    zipWith
      (\i src -> testCase ("Valid Example " ++ show i) $ assertValidSyntax src)
      [1 :: Int ..]
      validSyntaxExamples

invalidProgramSyntaxTests :: TestTree
invalidProgramSyntaxTests =
  testGroup "Invalid Syntax" $
    zipWith
      (\i src -> testCase ("Invalid Example " ++ show i) $ assertInvalidSyntax src)
      [1 :: Int ..]
      invalidSyntaxExamples
