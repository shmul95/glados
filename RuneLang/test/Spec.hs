module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Core.CLISpecs (cliTests)
import Core.LibSpecs (libTests)
import Core.PipelinesSpecs (pipelinesTests)

--
-- public
--

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ coreSpecs
      ]

--
-- private
--

coreSpecs :: TestTree
coreSpecs =
  testGroup
    "Core Tests"
    [ libTests,
      pipelinesTests,
      cliTests
    ]
