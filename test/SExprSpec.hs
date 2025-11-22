{-# LANGUAGE OverloadedStrings #-}

module SExprSpec (spec) where

import Test.Hspec
import SExpr

spec :: Spec
spec = do
  describe "SExpr data constructors" $ do
    it "creates Integer values" $ do
      let expr = Integer 42
      expr `shouldBe` Integer 42

    it "creates Symbol values" $ do
      let expr = Symbol "hello"
      expr `shouldBe` Symbol "hello"

    it "creates List values" $ do
      let expr = List [Integer 1, Symbol "x"]
      expr `shouldBe` List [Integer 1, Symbol "x"]

  describe "getSymbol" $ do
    it "extracts symbol from Symbol" $ do
      getSymbol (Symbol "test") `shouldBe` Just "test"
      getSymbol (Symbol "") `shouldBe` Just ""
      getSymbol (Symbol "complex-name!?") `shouldBe` Just "complex-name!?"

    it "returns Nothing for non-Symbol values" $ do
      getSymbol (Integer 42) `shouldBe` Nothing
      getSymbol (List []) `shouldBe` Nothing
      getSymbol (List [Integer 1]) `shouldBe` Nothing

  describe "getInteger" $ do
    it "extracts integer from Integer" $ do
      getInteger (Integer 42) `shouldBe` Just 42
      getInteger (Integer 0) `shouldBe` Just 0
      getInteger (Integer (-17)) `shouldBe` Just (-17)

    it "returns Nothing for non-Integer values" $ do
      getInteger (Symbol "test") `shouldBe` Nothing
      getInteger (List []) `shouldBe` Nothing
      getInteger (List [Integer 1]) `shouldBe` Nothing

  describe "getList" $ do
    it "extracts list from List" $ do
      getList (List []) `shouldBe` Just []
      getList (List [Integer 1]) `shouldBe` Just [Integer 1]
      getList (List [Integer 1, Symbol "x", List []]) `shouldBe`
        Just [Integer 1, Symbol "x", List []]

    it "returns Nothing for non-List values" $ do
      getList (Integer 42) `shouldBe` Nothing
      getList (Symbol "test") `shouldBe` Nothing

  describe "printTree" $ do
    it "prints Integer values" $ do
      printTree (Integer 42) `shouldBe` Just "a Number 42"
      printTree (Integer 0) `shouldBe` Just "a Number 0"
      printTree (Integer (-17)) `shouldBe` Just "a Number -17"

    it "prints Symbol values" $ do
      printTree (Symbol "test") `shouldBe` Just "a Symbol test"
      printTree (Symbol "") `shouldBe` Just "a Symbol "
      printTree (Symbol "complex-name!?") `shouldBe` Just "a Symbol complex-name!?"

    it "prints empty List" $ do
      printTree (List []) `shouldBe` Just "a List with nothing in it"

    it "prints List with single element" $ do
      printTree (List [Integer 42]) `shouldBe` Just "a List with a Number 42 followed by "
      printTree (List [Symbol "x"]) `shouldBe` Just "a List with a Symbol x followed by "

    it "prints List with multiple elements" $ do
      printTree (List [Integer 1, Integer 2]) `shouldBe` 
        Just "a List with a Number 1 followed by a Number 2"
      printTree (List [Symbol "+", Integer 1, Integer 2]) `shouldBe`
        Just "a List with a Symbol + followed by a Number 1, a Number 2"

    it "prints nested Lists" $ do
      printTree (List [List [Integer 1]]) `shouldBe`
        Just "a List with a List with a Number 1 followed by  followed by "
      printTree (List [List [Integer 1], Integer 2]) `shouldBe`
        Just "a List with a List with a Number 1 followed by  followed by a Number 2"

  describe "describeList" $ do
    it "describes empty list" $ do
      describeList [] `shouldBe` Just "nothing in it"

    it "describes single element list" $ do
      describeList [Integer 42] `shouldBe` Just "a Number 42 followed by "
      describeList [Symbol "x"] `shouldBe` Just "a Symbol x followed by "

    it "describes multiple element list" $ do
      describeList [Integer 1, Integer 2] `shouldBe`
        Just "a Number 1 followed by a Number 2"
      describeList [Symbol "x", Integer 42, Symbol "y"] `shouldBe`
        Just "a Symbol x followed by a Number 42, a Symbol y"

  describe "describeListRest" $ do
    it "describes single element" $ do
      describeListRest [Integer 42] `shouldBe` Just "a Number 42"
      describeListRest [Symbol "x"] `shouldBe` Just "a Symbol x"

    it "describes multiple elements with commas" $ do
      describeListRest [Integer 1, Integer 2] `shouldBe`
        Just "a Number 1, a Number 2"
      describeListRest [Symbol "x", Integer 42, Symbol "y"] `shouldBe`
        Just "a Symbol x, a Number 42, a Symbol y"

    it "handles nested lists" $ do
      describeListRest [List [Integer 1], Integer 2] `shouldBe`
        Just "a List with a Number 1 followed by , a Number 2"

  describe "Show instance (deriving)" $ do
    it "shows Integer values using Show" $ do
      show (Integer 42) `shouldBe` "Integer 42"
      show (Integer (-17)) `shouldBe` "Integer (-17)"

    it "shows Symbol values using Show" $ do
      show (Symbol "test") `shouldBe` "Symbol \"test\""
      show (Symbol "") `shouldBe` "Symbol \"\""

    it "shows List values using Show" $ do
      show (List []) `shouldBe` "List []"
      show (List [Integer 1]) `shouldBe` "List [Integer 1]"
      show (List [Integer 1, Symbol "x"]) `shouldBe` "List [Integer 1,Symbol \"x\"]"

    it "shows nested structures using Show" $ do
      show (List [List [Integer 1], Symbol "x"]) `shouldBe`
        "List [List [Integer 1],Symbol \"x\"]"

  describe "Eq instance (deriving)" $ do
    it "compares Integer values for equality" $ do
      Integer 42 `shouldBe` Integer 42
      Integer 42 `shouldNotBe` Integer 43

    it "compares Symbol values for equality" $ do
      Symbol "test" `shouldBe` Symbol "test"
      Symbol "test" `shouldNotBe` Symbol "other"

    it "compares List values for equality" $ do
      List [Integer 1] `shouldBe` List [Integer 1]
      List [Integer 1] `shouldNotBe` List [Integer 2]
      List [] `shouldBe` List []

    it "compares complex nested structures" $ do
      let expr1 = List [Symbol "+", List [Integer 1, Integer 2]]
      let expr2 = List [Symbol "+", List [Integer 1, Integer 2]]
      let expr3 = List [Symbol "+", List [Integer 1, Integer 3]]
      expr1 `shouldBe` expr2
      expr1 `shouldNotBe` expr3