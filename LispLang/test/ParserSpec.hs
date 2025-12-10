{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (parse, errorBundlePretty)
import Lisp.Parser.Parser
import Lisp.SExpr.SExpr

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
  [ testCase "parses integers" $ do
      parseTest "42" @?= Right (List [Integer 42])

  , testCase "parses symbols" $ do
      parseTest "foo" @?= Right (List [Symbol "foo"])

  , testCase "parses lists" $ do
      parseTest "(+ 1 2)" @?= Right (List [List [Symbol "+", Integer 1, Integer 2]])
  ]

parseLispNumberTests :: TestTree
parseLispNumberTests = testGroup "parseLispNumber"
  [ testCase "parses positive integers" $ do
      parseTest "123" @?= Right (List [Integer 123])

  , testCase "parses negative integers" $ do
      parseTest "-456" @?= Right (List [Integer (-456)])

  , testCase "parses zero" $ do
      parseTest "0" @?= Right (List [Integer 0])
  ]

parseLispStringTests :: TestTree
parseLispStringTests = testGroup "parseLispString"
  [ testCase "parses symbols" $ do
      parseTest "hello" @?= Right (List [Symbol "hello"])

  , testCase "parses operators as symbols" $ do
      parseTest "+" @?= Right (List [Symbol "+"])
  ]

parseLispArrayTests :: TestTree
parseLispArrayTests = testGroup "parseLispArray"
  [ testCase "parses empty lists" $ do
      parseTest "()" @?= Right (List [List []])

  , testCase "parses single-element lists" $ do
      parseTest "(42)" @?= Right (List [List [Integer 42]])

  , testCase "parses multi-element lists" $ do
      parseTest "(+ 1 2)" @?= Right (List [List [Symbol "+", Integer 1, Integer 2]])

  , testCase "parses nested lists" $ do
      parseTest "((+ 1 2) 3)" @?= 
        Right (List [List [List [Symbol "+", Integer 1, Integer 2], Integer 3]])
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
        Left _ -> return ()
        Right _ -> fail "Should have failed on unmatched parentheses"

  , testCase "fails on extra closing parentheses" $ do
      case parseTest "(+ 1 2))" of
        Left _ -> return ()
        Right _ -> fail "Should have failed on extra closing parentheses"
  ]
