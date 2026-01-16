module PreprocessSpecs (preprocessTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Control.Monad (unless)
import Data.List (isInfixOf)

import Rune.Preprocess (preprocessUseStatements)

--
-- public
--

preprocessTests :: TestTree
preprocessTests =
  testGroup
    "Preprocess Specs"
    [ testBasicUseStatement,
      testExtensionlessUseStatement,
      testMultipleUseStatements,
      testDuplicateUseStatements,
      testCyclicUseStatements,
      testNestedUseStatements,
      testMixedContent,
      testEmptyFile,
      testNonExistentFile,
      testInvalidUseStatements,
      testWhitespaceHandling,
      testComplexFilenames
    ]

--
-- private
--

testBasicUseStatement :: TestTree
testBasicUseStatement = testCase "basic use statement" $
  withTempFiles [("test.sw", "def test() -> i32 { 42 }")] $ \dir -> do
    let content = "use test.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> 
        assertEqual "Should expand use statement" "def test() -> i32 { 42 }\n" expanded

testExtensionlessUseStatement :: TestTree
testExtensionlessUseStatement = testCase "extensionless use statement tries multiple extensions" $
  withTempFiles [("test.sw", "def test() -> i32 { 42 }")] $ \dir -> do
    let content = "use test;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> 
        assertEqual "Should expand use statement by finding .sw file" "def test() -> i32 { 42 }\n" expanded

testMultipleUseStatements :: TestTree
testMultipleUseStatements = testCase "multiple use statements" $
  withTempFiles [("a.sw", "def a() -> i32 { 1 }"), ("b.sw", "def b() -> i32 { 2 }")] $ \dir -> do
    let content = "use a.sw;\nuse b.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        assertBool "Should contain function a" ("def a() -> i32 { 1 }" `elem` lines expanded)
        assertBool "Should contain function b" ("def b() -> i32 { 2 }" `elem` lines expanded)

testDuplicateUseStatements :: TestTree
testDuplicateUseStatements = testCase "duplicate use statements are ignored" $
  withTempFiles [("dup.sw", "def unique() -> i32 { 123 }")] $ \dir -> do
    let content = "use dup.sw;\nuse dup.sw;\nuse dup.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        let occurrences = length $ filter (== "def unique() -> i32 { 123 }") (lines expanded)
        assertEqual "Should only include file once" 1 occurrences

testCyclicUseStatements :: TestTree
testCyclicUseStatements = testCase "cyclic use statements don't cause infinite recursion" $
  withTempFiles [("cycle_a.sw", "def a() -> i32 { 1 }\nuse cycle_b.sw;"),
                 ("cycle_b.sw", "def b() -> i32 { 2 }\nuse cycle_a.sw;")] $ \dir -> do
    let content = "use cycle_a.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        assertBool "Should contain function a" ("def a() -> i32 { 1 }" `isInfixOf` expanded)
        assertBool "Should contain function b" ("def b() -> i32 { 2 }" `isInfixOf` expanded)
        let aMatches = length $ filter ("def a() -> i32 { 1 }" `isInfixOf`) (lines expanded)
        let bMatches = length $ filter ("def b() -> i32 { 2 }" `isInfixOf`) (lines expanded)
        assertBool "Should include a at least once" (aMatches >= 1)
        assertBool "Should include b at least once" (bMatches >= 1)

testNestedUseStatements :: TestTree
testNestedUseStatements = testCase "nested use statements work correctly" $
  withTempFiles [("base.sw", "def base() -> i32 { 0 }"),
                 ("middle.sw", "def middle() -> i32 { 1 }\nuse base.sw;"),
                 ("top.sw", "def top() -> i32 { 2 }\nuse middle.sw;")] $ \dir -> do
    let content = "use top.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        assertBool "Should contain top function" ("def top() -> i32 { 2 }" `isInfixOf` expanded)
        assertBool "Should contain middle function" ("def middle() -> i32 { 1 }" `isInfixOf` expanded)
        assertBool "Should contain base function" ("def base() -> i32 { 0 }" `isInfixOf` expanded)

testMixedContent :: TestTree
testMixedContent = testCase "mixed content with use statements" $
  withTempFiles [("lib.sw", "def helper() -> i32 { 99 }")] $ \dir -> do
    let content = "def main() -> i32 {\n    use lib.sw;\n    helper()\n}\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        let lines' = lines expanded
        assertBool "Should contain main function" ("def main() -> i32 {" `elem` lines')
        assertBool "Should contain helper function" ("def helper() -> i32 { 99 }" `elem` lines')
        assertBool "Should contain helper call" ("    helper()" `elem` lines')

testEmptyFile :: TestTree
testEmptyFile = testCase "empty file handling" $
  withTempFiles [("empty.sw", "")] $ \dir -> do
    let content = "use empty.sw;\ndef main() -> i32 { 0 }\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        let lines' = filter (not . null) (lines expanded)
        assertEqual "Should only contain main function" ["def main() -> i32 { 0 }"] lines'

testNonExistentFile :: TestTree
testNonExistentFile = testCase "non-existent file error" $ do
    let content = "use nonexistent.sw;\n"
    result <- preprocessUseStatements [] content
    case result of
      Left err -> assertBool "Should mention file not found" ("nonexistent.sw" `elem` words err)
      Right _ -> fail "Should have failed for non-existent file"

testInvalidUseStatements :: TestTree
testInvalidUseStatements = testCase "invalid use statements are ignored" $ do
    let content = "use;\nuse file\ndef main() -> i32 { 0 }\nuse file.sw\n"
    result <- preprocessUseStatements ["."] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        let lines' = lines expanded
        assertEqual "Should preserve invalid lines and main function" 
                   ["use;", "use file", "def main() -> i32 { 0 }", "use file.sw"] lines'

testWhitespaceHandling :: TestTree
testWhitespaceHandling = testCase "whitespace handling in use statements" $
  withTempFiles [("spaces.sw", "def spaced() -> i32 { 42 }")] $ \dir -> do
    let content = "  use   spaces.sw  ;  \n\t use spaces.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> do
        let occurrences = length $ filter (== "def spaced() -> i32 { 42 }") (lines expanded)
        assertEqual "Should handle whitespace and ignore duplicate" 1 occurrences

testComplexFilenames :: TestTree
testComplexFilenames = testCase "complex filenames with spaces and paths" $
  withTempFiles [("path/to file.sw", "def complex() -> i32 { 1 }")] $ \dir -> do
    let content = "use path/to file.sw;\n"
    result <- preprocessUseStatements [dir] content
    case result of
      Left err -> fail $ "Unexpected error: " ++ err
      Right expanded -> 
        assertBool "Should handle complex filename" ("def complex() -> i32 { 1 }" `elem` lines expanded)

--
-- Helper functions
--

withTempFiles :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withTempFiles files action = withSystemTempDirectory "preprocess-test" $ \dir -> do
  mapM_ (createFile dir) files
  action dir
  where
    takeDirectory path = 
      let reversed = reverse path
          afterSlash = dropWhile (/= '/') reversed  
      in if null afterSlash then "." else reverse (drop 1 afterSlash)
    
    createFile dir (filename, content) = do
      let fullPath = dir </> filename
      -- Create directory if needed  
      let dirPath = takeDirectory filename
      unless (dirPath == "." || dirPath == filename) $ 
        createDirectoryIfMissing True (dir </> dirPath)
      writeFile fullPath content
