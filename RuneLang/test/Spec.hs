module Main (main) where

import Test.Tasty
import Test.Tasty.Runners (NumThreads(..))

import Core.CLISpecs (cliTests)
import Core.LibSpecs (libTests)
import Core.PipelinesSpecs (pipelinesTests)

--
-- public
--

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
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
    [ libTests
    , pipelinesTests
    , cliTests
    ]

