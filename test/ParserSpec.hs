{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import Parser
import SExpr

-- Helper function to test parsing
parseTest :: String -> Either String SExpr
parseTest input = case parse parseLispDocument "" input of
  Left err -> Left (errorBundlePretty err)
  Right result -> Right result

spec :: Spec
spec = do
  describe "parseLispDocument" $ do
    it "parses empty input" $ do
      parseTest "" `shouldBe` Right (List [])
      parseTest "   " `shouldBe` Right (List [])
      parseTest "\n\t " `shouldBe` Right (List [])

    it "parses single expressions" $ do
      parseTest "42" `shouldBe` Right (List [Integer 42])
      parseTest "hello" `shouldBe` Right (List [Symbol "hello"])
      parseTest "()" `shouldBe` Right (List [List []])

    it "parses multiple expressions" $ do
      parseTest "42 hello" `shouldBe` Right (List [Integer 42, Symbol "hello"])
      parseTest "1 2 3" `shouldBe` Right (List [Integer 1, Integer 2, Integer 3])

    it "handles whitespace correctly" $ do
      parseTest " 42 " `shouldBe` Right (List [Integer 42])
      parseTest "\n42\t" `shouldBe` Right (List [Integer 42])

  describe "parseLispNumber" $ do
    it "parses positive integers" $ do
      parseTest "0" `shouldBe` Right (List [Integer 0])
      parseTest "42" `shouldBe` Right (List [Integer 42])
      parseTest "12345" `shouldBe` Right (List [Integer 12345])

    it "parses negative integers" $ do
      parseTest "-1" `shouldBe` Right (List [Integer (-1)])
      parseTest "-42" `shouldBe` Right (List [Integer (-42)])
      parseTest "-12345" `shouldBe` Right (List [Integer (-12345)])

    it "handles whitespace around numbers" $ do
      parseTest " 42 " `shouldBe` Right (List [Integer 42])
      parseTest "  -17  " `shouldBe` Right (List [Integer (-17)])

    it "rejects numbers followed by letters" $ do
      case parseTest "42x" of
        Left _ -> return ()  -- Should fail because 42 can't be followed by x
        Right _ -> expectationFailure "Should have failed on number followed by letter"

  describe "parseLispString" $ do
    it "parses simple symbols" $ do
      parseTest "x" `shouldBe` Right (List [Symbol "x"])
      parseTest "hello" `shouldBe` Right (List [Symbol "hello"])
      parseTest "test123" `shouldBe` Right (List [Symbol "test123"])

    it "parses symbols starting with letters and containing numbers" $ do
      parseTest "x42" `shouldBe` Right (List [Symbol "x42"])
      parseTest "var1" `shouldBe` Right (List [Symbol "var1"])

    it "parses operators" $ do
      parseTest "+" `shouldBe` Right (List [Symbol "+"])
      parseTest "*" `shouldBe` Right (List [Symbol "*"])
      parseTest "/" `shouldBe` Right (List [Symbol "/"])

    it "parses special characters" $ do
      parseTest "#t" `shouldBe` Right (List [Symbol "#t"])
      parseTest "#f" `shouldBe` Right (List [Symbol "#f"])
      parseTest "eq?" `shouldBe` Right (List [Symbol "eq?"])
      parseTest "<" `shouldBe` Right (List [Symbol "<"])
      parseTest ">" `shouldBe` Right (List [Symbol ">"])
      parseTest "=" `shouldBe` Right (List [Symbol "="])
      parseTest "!" `shouldBe` Right (List [Symbol "!"])
      parseTest "_" `shouldBe` Right (List [Symbol "_"])

    it "parses complex symbols" $ do
      parseTest "complex-name" `shouldBe` Right (List [Symbol "complex-name"])
      parseTest "test_var" `shouldBe` Right (List [Symbol "test_var"])
      parseTest "func!?" `shouldBe` Right (List [Symbol "func!?"])

    it "handles case sensitivity" $ do
      parseTest "Hello" `shouldBe` Right (List [Symbol "Hello"])
      parseTest "WORLD" `shouldBe` Right (List [Symbol "WORLD"])
      parseTest "MixedCase" `shouldBe` Right (List [Symbol "MixedCase"])

  describe "parseLispArray" $ do
    it "parses empty lists" $ do
      parseTest "()" `shouldBe` Right (List [List []])
      parseTest "(  )" `shouldBe` Right (List [List []])

    it "parses single element lists" $ do
      parseTest "(42)" `shouldBe` Right (List [List [Integer 42]])
      parseTest "(hello)" `shouldBe` Right (List [List [Symbol "hello"]])

    it "parses multiple element lists" $ do
      parseTest "(1 2)" `shouldBe` Right (List [List [Integer 1, Integer 2]])
      parseTest "(+ 1 2)" `shouldBe` Right (List [List [Symbol "+", Integer 1, Integer 2]])

    it "parses nested lists" $ do
      parseTest "((1))" `shouldBe` Right (List [List [List [Integer 1]]])
      parseTest "(1 (2 3))" `shouldBe` 
        Right (List [List [Integer 1, List [Integer 2, Integer 3]]])

    it "handles whitespace in lists" $ do
      parseTest "( 1 2 )" `shouldBe` Right (List [List [Integer 1, Integer 2]])
      parseTest "(  +   1   2  )" `shouldBe` 
        Right (List [List [Symbol "+", Integer 1, Integer 2]])

    it "parses complex nested structures" $ do
      parseTest "(define x (+ 1 2))" `shouldBe`
        Right (List [List [Symbol "define", Symbol "x", 
                          List [Symbol "+", Integer 1, Integer 2]]])

  describe "parseLispValue choice order" $ do
    it "prioritizes lists over numbers when ambiguous" $ do
      parseTest "(42)" `shouldBe` Right (List [List [Integer 42]])

    it "properly separates numbers from symbols" $ do
      parseTest "42 x" `shouldBe` Right (List [Integer 42, Symbol "x"])
      parseTest "42" `shouldBe` Right (List [Integer 42])

  describe "complex expressions" $ do
    it "parses define expressions" $ do
      parseTest "(define x 5)" `shouldBe`
        Right (List [List [Symbol "define", Symbol "x", Integer 5]])

    it "parses function calls" $ do
      parseTest "(+ x 2)" `shouldBe`
        Right (List [List [Symbol "+", Symbol "x", Integer 2]])

    it "parses if expressions" $ do
      parseTest "(if (> x 0) x 0)" `shouldBe`
        Right (List [List [Symbol "if", 
                          List [Symbol ">", Symbol "x", Integer 0],
                          Symbol "x", Integer 0]])

    it "parses lambda expressions" $ do
      parseTest "(lambda (x) (+ x 1))" `shouldBe`
        Right (List [List [Symbol "lambda", 
                          List [Symbol "x"],
                          List [Symbol "+", Symbol "x", Integer 1]]])

    it "parses multiple statements" $ do
      parseTest "(define x 5)(+ x 2)" `shouldBe`
        Right (List [List [Symbol "define", Symbol "x", Integer 5],
                    List [Symbol "+", Symbol "x", Integer 2]])

  describe "error cases" $ do
    it "fails on unmatched parentheses" $ do
      case parseTest "(+ 1 2" of
        Left _ -> return ()  -- Should fail
        Right _ -> expectationFailure "Should have failed on unmatched parentheses"

    it "fails on extra closing parentheses" $ do
      case parseTest "(+ 1 2))" of
        Left _ -> return ()  -- Should fail  
        Right _ -> expectationFailure "Should have failed on extra closing parentheses"

    it "fails on invalid characters in certain contexts" $ do
      case parseTest "(" of
        Left _ -> return ()  -- Should fail
        Right _ -> expectationFailure "Should have failed on incomplete list"

    it "fails on numbers immediately followed by letters" $ do
      case parseTest "42x" of
        Left _ -> return ()  -- Should fail
        Right _ -> expectationFailure "Should have failed on number followed by letter"

    it "fails on standalone minus sign" $ do
      case parseTest "-" of
        Left _ -> return ()  -- Should fail
        Right _ -> expectationFailure "Should have failed on standalone minus"