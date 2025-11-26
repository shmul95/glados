{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Text.Megaparsec (parse, errorBundlePretty)
import Parser
import SExpr

-- Helper function to test parsing
parseTest :: String -> Either String SExpr
parseTest input = case parse parseLispDocument "" input of
  Left err -> Left (errorBundlePretty err)
  Right result -> Right result

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ parseLispDocumentTests
  , parseLispNumberTests
  , parseLispStringTests
  , parseLispArrayTests
  , complexExpressionTests
  , errorCaseTests
  ]

parseLispDocumentTests :: TestTree
parseLispDocumentTests = testGroup "parseLispDocument"
  [ testCase "parses empty input" $ do
      parseTest "" @?= Right (List [])
      parseTest "   " @?= Right (List [])
      parseTest "\n\t " @?= Right (List [])

  , testCase "parses single expressions" $ do
      parseTest "42" @?= Right (List [Integer 42])
      parseTest "hello" @?= Right (List [Symbol "hello"])
      parseTest "()" @?= Right (List [List []])

  , testCase "parses multiple expressions" $ do
      parseTest "42 hello" @?= Right (List [Integer 42, Symbol "hello"])
      parseTest "1 2 3" @?= Right (List [Integer 1, Integer 2, Integer 3])

  , testCase "handles whitespace correctly" $ do
      parseTest " 42 " @?= Right (List [Integer 42])
      parseTest "\n42\t" @?= Right (List [Integer 42])
  ]

parseLispNumberTests :: TestTree
parseLispNumberTests = testGroup "parseLispNumber"
  [ testCase "parses positive integers" $ do
      parseTest "0" @?= Right (List [Integer 0])
      parseTest "42" @?= Right (List [Integer 42])
      parseTest "12345" @?= Right (List [Integer 12345])

  , testCase "parses negative integers" $ do
      parseTest "-1" @?= Right (List [Integer (-1)])
      parseTest "-42" @?= Right (List [Integer (-42)])
      parseTest "-12345" @?= Right (List [Integer (-12345)])

  , testCase "handles whitespace around numbers" $ do
      parseTest " 42 " @?= Right (List [Integer 42])
      parseTest "  -17  " @?= Right (List [Integer (-17)])

  , testCase "rejects numbers followed by letters" $ do
      case parseTest "42x" of
        Left _ -> return ()  -- Should fail
        Right _ -> assertBool "Should have failed on number followed by letter" False
  ]

parseLispStringTests :: TestTree
parseLispStringTests = testGroup "parseLispString"
  [ testCase "parses simple symbols" $ do
      parseTest "x" @?= Right (List [Symbol "x"])
      parseTest "hello" @?= Right (List [Symbol "hello"])
      parseTest "test123" @?= Right (List [Symbol "test123"])

  , testCase "parses operators" $ do
      parseTest "+" @?= Right (List [Symbol "+"])
      parseTest "*" @?= Right (List [Symbol "*"])
      parseTest "/" @?= Right (List [Symbol "/"])

  , testCase "parses special characters" $ do
      parseTest "#t" @?= Right (List [Symbol "#t"])
      parseTest "#f" @?= Right (List [Symbol "#f"])
      parseTest "eq?" @?= Right (List [Symbol "eq?"])
  ]

parseLispArrayTests :: TestTree
parseLispArrayTests = testGroup "parseLispArray"
  [ testCase "parses empty lists" $ do
      parseTest "()" @?= Right (List [List []])
      parseTest "(  )" @?= Right (List [List []])

  , testCase "parses single element lists" $ do
      parseTest "(42)" @?= Right (List [List [Integer 42]])
      parseTest "(hello)" @?= Right (List [List [Symbol "hello"]])

  , testCase "parses multiple element lists" $ do
      parseTest "(1 2)" @?= Right (List [List [Integer 1, Integer 2]])
      parseTest "(+ 1 2)" @?= Right (List [List [Symbol "+", Integer 1, Integer 2]])

  , testCase "parses nested lists" $ do
      parseTest "((1))" @?= Right (List [List [List [Integer 1]]])
      parseTest "(1 (2 3))" @?= 
        Right (List [List [Integer 1, List [Integer 2, Integer 3]]])
  ]

complexExpressionTests :: TestTree
complexExpressionTests = testGroup "complex expressions"
  [ testCase "parses define expressions" $ do
      parseTest "(define x 5)" @?=
        Right (List [List [Symbol "define", Symbol "x", Integer 5]])

  , testCase "parses function calls" $ do
      parseTest "(+ x 2)" @?=
        Right (List [List [Symbol "+", Symbol "x", Integer 2]])

  , testCase "parses lambda expressions" $ do
      parseTest "(lambda (x) (+ x 1))" @?=
        Right (List [List [Symbol "lambda", 
                          List [Symbol "x"],
                          List [Symbol "+", Symbol "x", Integer 1]]])
  ]

errorCaseTests :: TestTree
errorCaseTests = testGroup "error cases"
  [ testCase "fails on unmatched parentheses" $ do
      case parseTest "(+ 1 2" of
        Left _ -> return ()  -- Should fail
        Right _ -> assertBool "Should have failed on unmatched parentheses" False

  , testCase "fails on extra closing parentheses" $ do
      case parseTest "(+ 1 2))" of
        Left _ -> return ()  -- Should fail
        Right _ -> assertBool "Should have failed on extra closing parentheses" False
  ]
