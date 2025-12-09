{-# LANGUAGE OverloadedStrings #-}

module SExprSpec (sexprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Lisp.SExpr.SExpr

sexprTests :: TestTree
sexprTests = testGroup "SExpr Tests"
  [ sexprDataConstructorTests
  , getSymbolTests
  , getIntegerTests
  , getListTests
  , printTreeTests
  , describeListTests
  , describeListRestTests
  , showInstanceTests
  , eqInstanceTests
  ]

sexprDataConstructorTests :: TestTree
sexprDataConstructorTests = testGroup "SExpr data constructors"
  [ testCase "creates Integer values" $ do
      let expr = Integer 42
      expr @?= Integer 42

  , testCase "creates Symbol values" $ do
      let expr = Symbol "hello"
      expr @?= Symbol "hello"

  , testCase "creates List values" $ do
      let expr = List [Integer 1, Symbol "x"]
      expr @?= List [Integer 1, Symbol "x"]
  ]

getSymbolTests :: TestTree
getSymbolTests = testGroup "getSymbol"
  [ testCase "extracts symbol from Symbol" $ do
      getSymbol (Symbol "test") @?= Just "test"
      getSymbol (Symbol "") @?= Just ""
      getSymbol (Symbol "complex-name!?") @?= Just "complex-name!?"

  , testCase "returns Nothing for non-Symbol values" $ do
      getSymbol (Integer 42) @?= Nothing
      getSymbol (List []) @?= Nothing
      getSymbol (List [Integer 1]) @?= Nothing
  ]

getIntegerTests :: TestTree
getIntegerTests = testGroup "getInteger"
  [ testCase "extracts integer from Integer" $ do
      getInteger (Integer 42) @?= Just 42
      getInteger (Integer 0) @?= Just 0
      getInteger (Integer (-17)) @?= Just (-17)

  , testCase "returns Nothing for non-Integer values" $ do
      getInteger (Symbol "test") @?= Nothing
      getInteger (List []) @?= Nothing
      getInteger (List [Integer 1]) @?= Nothing
  ]

getListTests :: TestTree
getListTests = testGroup "getList"
  [ testCase "extracts list from List" $ do
      getList (List []) @?= Just []
      getList (List [Integer 1]) @?= Just [Integer 1]
      getList (List [Integer 1, Symbol "x", List []]) @?=
        Just [Integer 1, Symbol "x", List []]

  , testCase "returns Nothing for non-List values" $ do
      getList (Integer 42) @?= Nothing
      getList (Symbol "test") @?= Nothing
  ]

printTreeTests :: TestTree
printTreeTests = testGroup "printTree"
  [ testCase "prints Integer values" $ do
      printTree (Integer 42) @?= Just "a Number 42"
      printTree (Integer 0) @?= Just "a Number 0"
      printTree (Integer (-17)) @?= Just "a Number -17"

  , testCase "prints Symbol values" $ do
      printTree (Symbol "test") @?= Just "a Symbol test"
      printTree (Symbol "") @?= Just "a Symbol "
      printTree (Symbol "complex-name!?") @?= Just "a Symbol complex-name!?"

  , testCase "prints empty List" $ do
      printTree (List []) @?= Just "a List with nothing in it"

  , testCase "prints List with single element" $ do
      printTree (List [Integer 42]) @?= Just "a List with a Number 42 followed by "
      printTree (List [Symbol "x"]) @?= Just "a List with a Symbol x followed by "

  , testCase "prints List with multiple elements" $ do
      printTree (List [Integer 1, Integer 2]) @?= 
        Just "a List with a Number 1 followed by a Number 2"
      printTree (List [Symbol "+", Integer 1, Integer 2]) @?=
        Just "a List with a Symbol + followed by a Number 1, a Number 2"
  ]

describeListTests :: TestTree
describeListTests = testGroup "describeList"
  [ testCase "describes empty list" $ do
      describeList [] @?= Just "nothing in it"

  , testCase "describes single element list" $ do
      describeList [Integer 42] @?= Just "a Number 42 followed by "
      describeList [Symbol "x"] @?= Just "a Symbol x followed by "

  , testCase "describes multiple element list" $ do
      describeList [Integer 1, Integer 2] @?=
        Just "a Number 1 followed by a Number 2"
      describeList [Symbol "x", Integer 42, Symbol "y"] @?=
        Just "a Symbol x followed by a Number 42, a Symbol y"
  ]

describeListRestTests :: TestTree
describeListRestTests = testGroup "describeListRest"
  [ testCase "describes single element" $ do
      describeListRest [Integer 42] @?= Just "a Number 42"
      describeListRest [Symbol "x"] @?= Just "a Symbol x"

  , testCase "describes multiple elements with commas" $ do
      describeListRest [Integer 1, Integer 2] @?=
        Just "a Number 1, a Number 2"
      describeListRest [Symbol "x", Integer 42, Symbol "y"] @?=
        Just "a Symbol x, a Number 42, a Symbol y"
  ]

showInstanceTests :: TestTree
showInstanceTests = testGroup "Show instance (deriving)"
  [ testCase "shows Integer values using Show" $ do
      show (Integer 42) @?= "Integer 42"
      show (Integer (-17)) @?= "Integer (-17)"

  , testCase "shows Symbol values using Show" $ do
      show (Symbol "test") @?= "Symbol \"test\""
      show (Symbol "") @?= "Symbol \"\""

  , testCase "shows List values using Show" $ do
      show (List []) @?= "List []"
      show (List [Integer 1]) @?= "List [Integer 1]"
      show (List [Integer 1, Symbol "x"]) @?= "List [Integer 1,Symbol \"x\"]"
  ]

eqInstanceTests :: TestTree
eqInstanceTests = testGroup "Eq instance (deriving)"
  [ testCase "compares Integer values for equality" $ do
      (Integer 42 == Integer 42) @?= True
      (Integer 42 == Integer 43) @?= False

  , testCase "compares Symbol values for equality" $ do
      (Symbol "test" == Symbol "test") @?= True
      (Symbol "test" == Symbol "other") @?= False

  , testCase "compares List values for equality" $ do
      (List [Integer 1] == List [Integer 1]) @?= True
      (List [Integer 1] == List [Integer 2]) @?= False
      (List [] == List []) @?= True
  ]
