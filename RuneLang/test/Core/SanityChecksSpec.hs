module Core.SanityChecksSpec (sanityChecksTests) where

import Rune.SanityChecks (performSanityChecksWith, checkRequiredTools, checkArchitecture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

sanityChecksTests :: TestTree
sanityChecksTests =
  testGroup
    "Sanity Checks Tests"
    [
        checkRequiredToolsTest,
        checkArchitectureTest,
        performSanityChecksTest
    ]

checkRequiredToolsTest :: TestTree
checkRequiredToolsTest =
    testGroup
        "Required Tools Tests"
        [
            testCase "Both tools present" $ do
                result <- checkRequiredTools mockHasToolSuccess
                result @?= Right ()
            ,
            testCase "GCC missing" $ do
                result <- checkRequiredTools mockHasToolFailGcc
                result @?= Left "'gcc' not found in PATH. Please install GCC to proceed."
            ,
            testCase "NASM missing" $ do
                result <- checkRequiredTools mockHasToolFailNasm
                result @?= Left "'nasm' not found in PATH. Please install NASM to proceed."
            ,
            testCase "Both tools missing" $ do
                result <- checkRequiredTools mockHasToolFail
                result @?= Left "'gcc' not found in PATH. Please install GCC to proceed."
        ]

checkArchitectureTest :: TestTree
checkArchitectureTest =
    testGroup
        "Required Host Architecture Tests"
        [
            testCase "Supported architecture" $ do
                result <- checkArchitecture "x86_64" mockIsArchSupportedSuccess
                result @?= Right ()
            ,
            testCase "Unsupported architecture" $ do
                result <- checkArchitecture "unsupported_arch" mockIsArchSupportedFail
                result @?= Left "Error: Unsupported architecture 'unsupported_arch'. "
        ]

performSanityChecksTest :: TestTree
performSanityChecksTest =
    testGroup
        "Perform Sanity Checks Tests"
        [
            testCase "All checks pass" $ do
                result <- performSanityChecksWith "x86_64" mockIsArchSupportedSuccess mockHasToolSuccess
                result @?= Right ()
            ,
            testCase "Architecture check fails" $ do
                result <- performSanityChecksWith "unsupported_arch" mockIsArchSupportedFail mockHasToolSuccess
                result @?= Left "Error: Unsupported architecture 'unsupported_arch'. "
            ,
            testCase "Tool check fails" $ do
                result <- performSanityChecksWith "x86_64" mockIsArchSupportedSuccess mockHasToolFailGcc
                result @?= Left "'gcc' not found in PATH. Please install GCC to proceed."
        ]

---
--- private mock functions
---

mockHasToolSuccess :: String -> IO Bool
mockHasToolSuccess _ = return True

mockHasToolFail :: String -> IO Bool
mockHasToolFail _ = return False

mockHasToolFailGcc :: String -> IO Bool
mockHasToolFailGcc "gcc" = return False
mockHasToolFailGcc _     = return True

mockHasToolFailNasm :: String -> IO Bool
mockHasToolFailNasm "nasm" = return False
mockHasToolFailNasm _      = return True

mockIsArchSupportedSuccess :: String -> Bool
mockIsArchSupportedSuccess _ = True

mockIsArchSupportedFail :: String -> Bool
mockIsArchSupportedFail _ = False
