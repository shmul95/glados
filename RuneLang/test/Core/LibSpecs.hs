module Core.LibSpecs (libTests) where

import Lib (escapeString, isPrintable, fixpoint, alignTo, align8, alignSize)
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
      escapeStringTests,
      fixpointTests,
      alignToTests,
      align8Tests,
      alignSizeTests
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

fixpointTests :: TestTree
fixpointTests =
  testGroup
    "fixpoint Tests"
    [ testCase "Fixpoint of incrementing function on integers" $
        let f :: Int -> Int
            f x = if x < 5 then x + 1 else x
            result = fixpoint f 0
        in assertEqual "Should be 5" 5 result
    ]

alignToTests :: TestTree
alignToTests =
  testGroup
    "alignTo Tests"
    [ testCase "Align 5 to 4 should be 8" $
        assertEqual "Should be 8" 8 (alignTo 4 5),
      testCase "Align 16 to 8 should be 16" $
        assertEqual "Should be 16" 16 (alignTo 8 16),
      testCase "Align 17 to 8 should be 24" $
        assertEqual "Should be 24" 24 (alignTo 8 17),
      testCase "Align 0 to any should be 0" $
        assertEqual "Should be 0" 0 (alignTo 7 0)
    ]

align8Tests :: TestTree
align8Tests =
  testGroup
    "align8 Tests"
    [ testCase "Align 5 to 8 should be 8" $
        assertEqual "Should be 8" 8 (align8 5),
      testCase "Align 16 to 8 should be 16" $
        assertEqual "Should be 16" 16 (align8 16),
      testCase "Align 17 to 8 should be 24" $
        assertEqual "Should be 24" 24 (align8 17),
      testCase "Align 0 to 8 should be 0" $
        assertEqual "Should be 0" 0 (align8 0)
    ]

alignSizeTests :: TestTree
alignSizeTests =
  testGroup
    "alignSize Tests"
    [ testCase "Align size 0 should be 1" $
        assertEqual "Should be 1" 1 (alignSize 0),
      testCase "Align size 1 should be 1" $
        assertEqual "Should be 1" 1 (alignSize 1),
      testCase "Align size 5 should be 5" $
        assertEqual "Should be 5" 5 (alignSize 5),
      testCase "Align size 8 should be 8" $
        assertEqual "Should be 8" 8 (alignSize 8),
      testCase "Align size 10 should be 8" $
        assertEqual "Should be 8" 8 (alignSize 10)
    ]
