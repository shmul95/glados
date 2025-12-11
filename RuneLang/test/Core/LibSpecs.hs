module Core.LibSpecs (libTests) where

import Lib (escapeString, isPrintable)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Data.Char (chr)

--
-- public
--

libTests :: TestTree
libTests =
  testGroup
    "Lib Tests"
    [ isPrintableTests,
      escapeStringTests
    ]

--
-- private
--

isPrintableTests :: TestTree
isPrintableTests =
  testGroup
    "isPrintable Tests"
    [ testCase "Space ' ' is printable (lower boundary)" $
        assertBool "Should be True" (isPrintable ' '),
      testCase "Tilde '~' is printable (upper boundary)" $
        assertBool "Should be True" (isPrintable '~'),
      testCase "A normal letter 'a' is printable" $
        assertBool "Should be True" (isPrintable 'a'),
      testCase "Null character '\\0' is not printable (below lower boundary)" $
        assertBool "Should be False" (not (isPrintable '\0')),
      testCase "Control char (US, char 31) is not printable (just below space)" $
        assertBool "Should be False" (not (isPrintable (chr 31))),
      testCase "DEL char (char 127) is not printable (just above tilde)" $
        assertBool "Should be False" (not (isPrintable (chr 127)))
    ]

escapeStringTests :: TestTree
escapeStringTests =
  testGroup
    "escapeString Tests"
    [ testCase "Escapes null character '\\0'" $
        assertEqual "Should be \"\\0\"" "\\0" (escapeString "\0"),
      testCase "Escapes newline '\\n'" $
        assertEqual "Should be \"\\n\"" "\\n" (escapeString "\n"),
      testCase "Escapes tab '\\t'" $
        assertEqual "Should be \"\\t\"" "\\t" (escapeString "\t"),
      testCase "Escapes carriage return '\\r'" $
        assertEqual "Should be \"\\r\"" "\\r" (escapeString "\r"),
      testCase "Escapes backslash '\\\\'" $
        assertEqual "Should be \"\\\\\"" "\\\\" (escapeString "\\"),
      testCase "Escapes double quote '\"'" $
        assertEqual "Should be \"\\\"\"" "\\\"" (escapeString "\""),
      testCase "Does not escape normal characters" $
        assertEqual "Should be unchanged" "abc" (escapeString "abc"),
      testCase "Empty string returns empty string" $
        assertEqual "Should be \"\"" "" (escapeString ""),
      testCase "Escapes a string with mixed special and normal chars" $
        let input = "a\t\n\r\\\"b\0c"
            expected = "a\\t\\n\\r\\\\\\\"b\\0c"
         in assertEqual "Should escape all special characters" expected (escapeString input)
    ]
