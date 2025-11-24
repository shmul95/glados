module PipelinesSpec (pipelinesTests) where

import Control.Monad (when)
import Rune.Pipelines (compilePipeline, interpretPipeline)
import System.Directory (doesFileExist, removeFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

pipelinesTests :: TestTree
pipelinesTests =
  testGroup
    "Rune Pipelines Tests"
    [ compilePipelineTests,
      interpretPipelineTests
    ]

--
-- private helpers
--

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = doesFileExist path >>= (`when` removeFile path)

compilePipelineTests :: TestTree
compilePipelineTests = testCase "compilePipeline (hello_rune.ru)" $ do
  let inFile = "examples/hello_rune.ru"
  let outFile = "out.ir.test"
  compilePipeline inFile outFile
  removeFileIfExists outFile
  () @?= ()

interpretPipelineTests :: TestTree
interpretPipelineTests = testCase "interpretPipeline (hello_rune.ru)" $ do
  let inFile = "examples/hello_rune.ru"
  interpretPipeline inFile
  () @?= ()
