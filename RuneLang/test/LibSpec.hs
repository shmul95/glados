module LibSpec (libTests) where

import Lib
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

libTests :: TestTree
libTests =
  testGroup
    "Lib Specs"
    [ testEscapeString
    ]

testEscapeString :: TestTree
testEscapeString =
  testGroup
    "escapeString"
    [ testCase "string without special chars" $ escapeString "hello" @?= "hello",
      testCase "string with backslash" $ escapeString "test\\path" @?= "test\\\\path",
      testCase "string with newline" $ escapeString "line1\nline2" @?= "line1\\nline2",
      testCase "string with tab" $ escapeString "col1\tcol2" @?= "col1\\tcol2",
      testCase "string with carriage return" $ escapeString "text\rmore" @?= "text\\rmore",
      testCase "string with quote" $ escapeString "say \"hi\"" @?= "say \\\"hi\\\"",
      testCase "string with null" $ escapeString "a\0b" @?= "a\\0b",
      testCase "empty string" $ escapeString "" @?= "",
      testCase "mixed special chars" $ escapeString "a\\b\nc\"d" @?= "a\\\\b\\nc\\\"d",
      testCase "all special chars" $ escapeString "\0\n\t\r\\\"" @?= "\\0\\n\\t\\r\\\\\\\""
    ]
