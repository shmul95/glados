module ASTSpec (astTests) where

import Control.Exception (try)
import Rune.AST.Parser.Parser (parseRune)
import Rune.AST.Printer (prettyPrint)
import Rune.Lexer.Lexer (lexer)
import System.IO.Error (isDoesNotExistError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Text.Megaparsec (errorBundlePretty)

--
-- public
--

astTests :: TestTree
astTests =
  testGroup
    "Rune AST Parsing and Printing Tests"
    [ examplesFilesTests
    ]

--
-- private
--

checkFileAST :: FilePath -> String -> Assertion
checkFileAST filePath expected = do
  contentResult <- try $ readFile filePath :: IO (Either IOError String)
  case contentResult of
    Left ex | isDoesNotExistError ex -> assertFailure $ "File not found: " ++ filePath
    Left ex -> assertFailure $ "Failed to read file " ++ filePath ++ ": " ++ show ex
    Right content ->
      case lexer filePath content of
        Left err -> assertFailure $ "Lexing failed for " ++ filePath ++ ":\n" ++ errorBundlePretty err
        Right tokens ->
          case parseRune filePath tokens of
            Left err -> assertFailure $ "Parsing failed for " ++ filePath ++ ":\n" ++ err
            Right ast ->
              let actual = prettyPrint ast
                  actualCleaned = reverse $ dropWhile (`elem` "\n\r") $ reverse actual
               in actualCleaned @?= expected

examplesFilesTests :: TestTree
examplesFilesTests =
  testGroup
    "AST from Examples"
    [ testCase "hello_rune.ru" $ checkFileAST "examples/hello_rune.ru" expectedHelloRune,
      testCase "return_0.ru" $ checkFileAST "examples/return_0.ru" expectedReturn0,
      testCase "fibonacci.ru" $ checkFileAST "examples/fibonacci.ru" expectedFibonacci,
      testCase "strings.ru" $ checkFileAST "examples/strings.ru" expectedStrings,
      testCase "error_handling.ru" $ checkFileAST "examples/error_handling.ru" expectedErrorHandling,
      testCase "factorial.ru" $ checkFileAST "examples/factorial.ru" expectedFactorial
    ]

--
-- expected AST structures (pretty-printed output)
--

expectedHelloRune :: String
expectedHelloRune =
  "Program: examples/hello_rune.ru\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: null\n"
    ++ "    Body:\n"
    ++ "      StmtExpr\n"
    ++ "        ExprCall show\n"
    ++ "        Arguments:\n"
    ++ "          ExprLitString \"Hello, Rune!\\n\""

expectedReturn0 :: String
expectedReturn0 =
  "Program: examples/return_0.ru\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: i32\n"
    ++ "    Body:\n"
    ++ "      StmtExpr\n"
    ++ "        ExprLitInt 0"

expectedFibonacci :: String
expectedFibonacci =
  "Program: examples/fibonacci.ru\n"
    ++ "  DefFunction fibonacci\n"
    ++ "    Parameters:\n"
    ++ "      n: i32\n"
    ++ "    ReturnType: i32\n"
    ++ "    Body:\n"
    ++ "      StmtIf\n"
    ++ "        Condition:\n"
    ++ "          ExprBinary <=\n"
    ++ "            ExprVar n\n"
    ++ "            ExprLitInt 0\n"
    ++ "        Then:\n"
    ++ "          StmtExpr\n"
    ++ "            ExprLitInt 0\n"
    ++ "      StmtIf\n"
    ++ "        Condition:\n"
    ++ "          ExprBinary ==\n"
    ++ "            ExprVar n\n"
    ++ "            ExprLitInt 1\n"
    ++ "        Then:\n"
    ++ "          StmtExpr\n"
    ++ "            ExprLitInt 1\n"
    ++ "      StmtExpr\n"
    ++ "        ExprBinary +\n"
    ++ "          ExprCall fibonacci\n"
    ++ "          Arguments:\n"
    ++ "            ExprBinary -\n"
    ++ "              ExprVar n\n"
    ++ "              ExprLitInt 1\n"
    ++ "          ExprCall fibonacci\n"
    ++ "          Arguments:\n"
    ++ "            ExprBinary -\n"
    ++ "              ExprVar n\n"
    ++ "              ExprLitInt 2\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: i32\n"
    ++ "    Body:\n"
    ++ "      StmtExpr\n"
    ++ "        ExprCall fibonacci\n"
    ++ "        Arguments:\n"
    ++ "          ExprLitInt 10"

expectedStrings :: String
expectedStrings =
  "Program: examples/strings.ru\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: null\n"
    ++ "    Body:\n"
    ++ "      StmtVarDecl str : string\n"
    ++ "      Value:\n"
    ++ "        ExprLitString \"Warszawa\"\n"
    ++ "      StmtForEach samuel\n"
    ++ "        Iterable:\n"
    ++ "          ExprVar str\n"
    ++ "        Body:\n"
    ++ "          StmtExpr\n"
    ++ "            ExprCall show\n"
    ++ "            Arguments:\n"
    ++ "              ExprVar samuel"

expectedErrorHandling :: String
expectedErrorHandling =
  "Program: examples/error_handling.ru\n"
    ++ "  DefFunction maybe_function\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: i32\n"
    ++ "    Body:\n"
    ++ "      StmtVarDecl num\n"
    ++ "      Value:\n"
    ++ "        ExprLitInt 42\n"
    ++ "      StmtIf\n"
    ++ "        Condition:\n"
    ++ "          ExprBinary ==\n"
    ++ "            ExprVar num\n"
    ++ "            ExprLitInt 42\n"
    ++ "        Then:\n"
    ++ "          StmtReturn\n"
    ++ "            ExprVar num\n"
    ++ "      StmtReturn\n"
    ++ "        ExprCall error\n"
    ++ "        Arguments:\n"
    ++ "          ExprLitString \"An error occurred\"\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: i32\n"
    ++ "    Body:\n"
    ++ "      StmtVarDecl a\n"
    ++ "      Value:\n"
    ++ "        ExprUnary ?\n"
    ++ "          ExprCall maybe_function\n"
    ++ "          Arguments:\n"
    ++ "      StmtReturn\n"
    ++ "        ExprLitInt 0"

expectedFactorial :: String
expectedFactorial =
  "Program: examples/factorial.ru\n"
    ++ "  DefFunction factorial\n"
    ++ "    Parameters:\n"
    ++ "      n: i32\n"
    ++ "    ReturnType: i32\n"
    ++ "    Body:\n"
    ++ "      StmtIf\n"
    ++ "        Condition:\n"
    ++ "          ExprBinary <\n"
    ++ "            ExprVar n\n"
    ++ "            ExprLitInt 0\n"
    ++ "        Then:\n"
    ++ "          StmtReturn\n"
    ++ "            ExprCall error\n"
    ++ "            Arguments:\n"
    ++ "              ExprLitString \"Cannot calculate factorial of negative number\"\n"
    ++ "      StmtIf\n"
    ++ "        Condition:\n"
    ++ "          ExprBinary ==\n"
    ++ "            ExprVar n\n"
    ++ "            ExprLitInt 0\n"
    ++ "        Then:\n"
    ++ "          StmtExpr\n"
    ++ "            ExprLitInt 1\n"
    ++ "        Else:\n"
    ++ "          StmtVarDecl res\n"
    ++ "          Value:\n"
    ++ "            ExprUnary ?\n"
    ++ "              ExprCall factorial\n"
    ++ "              Arguments:\n"
    ++ "                ExprBinary -\n"
    ++ "                  ExprVar n\n"
    ++ "                  ExprLitInt 1\n"
    ++ "          StmtExpr\n"
    ++ "            ExprBinary *\n"
    ++ "              ExprVar n\n"
    ++ "              ExprVar res\n"
    ++ "  DefFunction main\n"
    ++ "    Parameters:\n"
    ++ "    ReturnType: null\n"
    ++ "    Body:\n"
    ++ "      StmtVarDecl val\n"
    ++ "      Value:\n"
    ++ "        ExprUnary ?\n"
    ++ "          ExprCall factorial\n"
    ++ "          Arguments:\n"
    ++ "            ExprLitInt 5\n"
    ++ "      StmtExpr\n"
    ++ "        ExprCall show\n"
    ++ "        Arguments:\n"
    ++ "          ExprVar val"
