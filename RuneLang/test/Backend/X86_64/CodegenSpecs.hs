{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.CodegenSpecs (codegenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Rune.Backend.X86_64.Codegen
import Rune.IR.Nodes (IRType(..), IROperand(..))

--
-- public
--

codegenTests :: TestTree
codegenTests = testGroup "Rune.Backend.X86_64.Codegen"
  [
    commaSepTests,
    showStaticOperandTests,
    getDataDirectiveTests,
    isStaticOperandTests,
    collectStaticArraysTests
  ]

--
-- private
--

collectStaticArraysTests :: TestTree
collectStaticArraysTests = testGroup "collectStaticArrays"
  [
  ]

isStaticOperandTests :: TestTree
isStaticOperandTests = testGroup "isStaticOperand"
  [
    testCase "IRConstInt" $
      assertBool "should be static" $
        isStaticOperand (IRConstInt 42) == True
  , testCase "IRConstChar" $
      assertBool "should be static" $
        isStaticOperand (IRConstChar 'A') == True
  , testCase "IRConstBool" $
      assertBool "should be static" $
        isStaticOperand (IRConstBool True) == True
  , testCase "IRConstNull" $
      assertBool "should be static" $
        isStaticOperand IRConstNull == True
  , testCase "IRGlobal" $
      assertBool "should be static" $
        isStaticOperand (IRGlobal "global_var" IRI32) == True
  , testCase "Other operand" $
      assertBool "should not be static" $
        isStaticOperand (IRTemp "temp1" IRI32) == False
  ]

getDataDirectiveTests :: TestTree
getDataDirectiveTests = testGroup "getDataDirective"
  [
    testCase "IRI8" $
      assertBool "should return db" $
        getDataDirective IRI8 == "db"
  , testCase "IRI16" $
      assertBool "should return dw" $
        getDataDirective IRI16 == "dw"
  , testCase "IRI32" $
      assertBool "should return dd" $
        getDataDirective IRI32 == "dd"
  , testCase "IRI64" $
      assertBool "should return dq" $
        getDataDirective IRI64 == "dq"
  , testCase "Array type" $
      assertBool "should return dq for array" $
        getDataDirective (IRArray IRI64 10) == "dq"
  ]

showStaticOperandTests :: TestTree
showStaticOperandTests = testGroup "showStaticOperand"
  [
    testCase "IRConstInt" $
      assertBool "should show integer" $
        showStaticOperand IRI32 (IRConstInt 42) == "42"
  , testCase "IRConstChar" $
      assertBool "should show char as integer" $
        showStaticOperand IRChar (IRConstChar 'A') == "65"
  , testCase "IRConstBool True" $
      assertBool "should show true as 1" $
        showStaticOperand IRBool (IRConstBool True) == "1"
  , testCase "IRConstBool False" $
      assertBool "should show false as 0" $
        showStaticOperand IRBool (IRConstBool False) == "0"
  , testCase "IRConstNull" $
      assertBool "should show null as 0" $
        showStaticOperand IRNull IRConstNull == "0"
  , testCase "IRGlobal" $
      assertBool "should show global name" $
        showStaticOperand IRI32 (IRGlobal "global_var" IRI32) == "global_var"
  , testCase "Other operand" $
      assertBool "should default to 0" $
        showStaticOperand IRI32 (IRTemp "temp1" IRI32) == "0"
  ]

commaSepTests :: TestTree
commaSepTests = testGroup "commaSep"
  [
    testCase "empty list" $
      assertBool "should be empty string" $
        commaSep [] == ""
  , testCase "single element" $
      assertBool "should be single element" $
        commaSep ["a"] == "a"
  , testCase "multiple elements" $
      assertBool "should be comma separated" $
        commaSep ["a", "b", "c"] == "a, b, c"
  ]
