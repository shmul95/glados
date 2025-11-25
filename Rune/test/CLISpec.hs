module CLISpec (cliTests) where

import CLI (Action (..), parseArgs, runCLI)
import Control.Exception (evaluate)
import Data.List (isInfixOf)
import System.IO.Silently (capture_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

--
-- public
--

cliTests :: TestTree
cliTests =
  testGroup
    "CLI Argument Parsing Tests"
    [ parseArgsTests,
      runCLITests,
      actionTests
    ]

--
-- helpers
--

expectedUsage :: String
expectedUsage =
  unlines
    [ "Usage: glados <command> [file] [options]",
      "",
      "Commands:",
      "  help           Show this help message",
      "  build [file]   Compile the given source file",
      "  run   [file]   Interpret the given source file",
      "",
      "Options:",
      "  -o, --output   Specify the output file for compilation"
    ]

--
-- private
--

parseArgsTests :: TestTree
parseArgsTests =
  testGroup
    "parseArgs"
    [ noCommandTest,
      invalidCommandTest,
      helpCommandTest,
      runCommandTests,
      buildCommandTests
    ]

runCLITests :: TestTree
runCLITests =
  testGroup
    "Run CLI Tests"
    [ testCase "ShowUsage prints usage" $ do
        output <- capture_ $ runCLI ShowUsage
        assertBool "Expected usage not found" (expectedUsage `isInfixOf` output)

        -- testCase "Interpret runs interpretPipeline" $ do
        --   -- on capture pour éviter du bruit sur stdout/stderr pendant le test
        --   _ <- capture_ $ runCLI (Interpret "examples/hello_rune.ru")
        --   () @?= (), -- assertion triviale : l'appel ne doit pas lever d'exception
        -- testCase "Compile with explicit output file" $ do
        --   let outFile = "out.ir.test.cli"
        --   _ <- capture_ $ runCLI (Compile "examples/hello_rune.ru" (Just outFile))
        --   exists <- doesFileExist outFile
        --   -- vérifier que le fichier de sortie a bien été créé
        --   assertBool ("Expected output file created: " ++ outFile) exists
        --   when exists (removeFile outFile),
        -- testCase "Compile with Nothing uses default 'out'" $ do
        --   let defaultOut = "out"
        --   _ <- capture_ $ runCLI (Compile "examples/hello_rune.ru" Nothing)
        --   exists <- doesFileExist defaultOut
        --   assertBool ("Expected default output file created: " ++ defaultOut) exists
        --   when exists (removeFile defaultOut)
    ]

actionTests :: TestTree
actionTests =
  testGroup
    "Action Tests"
    [ showActionTest,
      eqActionTest
    ]

--
-- parse args
--

helpCommandTest :: TestTree
helpCommandTest =
  testGroup
    "Help Command Tests"
    [ testCase "Help command (help)" $
        parseArgs ["help"] @?= Right ShowUsage,
      testCase "Help command (--help)" $
        parseArgs ["--help"] @?= Right ShowUsage,
      testCase "Help command (-h)" $
        parseArgs ["-h"] @?= Right ShowUsage
    ]

runCommandTests :: TestTree
runCommandTests =
  testGroup
    "Run Command Tests"
    [ testCase "Run with file (run)" $
        parseArgs ["run", "source.ru"] @?= Right (Interpret "source.ru"),
      testCase "Run with file (-r)" $
        parseArgs ["-r", "test.ru"] @?= Right (Interpret "test.ru"),
      testCase "Run with file (--run)" $
        parseArgs ["--run", "example.ru"] @?= Right (Interpret "example.ru"),
      testCase "Run without file (Error)" $
        parseArgs ["run"] @?= Left "The 'run' command requires an input file.",
      testCase "Run with too many args (Error)" $
        parseArgs ["run", "f1.ru", "f2.ru"] @?= Left "The 'run' command takes exactly one file argument."
    ]

buildCommandTests :: TestTree
buildCommandTests =
  testGroup
    "Build Command Tests"
    [ testCase "Build with file, no output (build)" $
        parseArgs ["build", "source.ru"] @?= Right (Compile "source.ru" Nothing),
      testCase "Build with file, short output (-b -o)" $
        parseArgs ["-b", "input.ru", "-o", "out.ir"] @?= Right (Compile "input.ru" (Just "out.ir")),
      testCase "Build with file, long build (--build -output)" $
        parseArgs ["--build", "input.ru", "--output", "out.ir"] @?= Right (Compile "input.ru" (Just "out.ir")),
      testCase "Build with file, long output (build --output)" $
        parseArgs ["build", "source.ru", "--output", "custom.ir"] @?= Right (Compile "source.ru" (Just "custom.ir")),
      testCase "Build without file (Error)" $
        parseArgs ["build"] @?= Left "The 'build' command requires an input file.",
      testCase "Build with invalid args (Error: -o first)" $
        parseArgs ["build", "-o", "out.ir"] @?= Left "Invalid arguments for build command: -o out.ir",
      testCase "Build with too many args (Error)" $
        parseArgs ["build", "f1.ru", "f2.ru", "f3.ru"] @?= Left "Invalid arguments for build command: f1.ru f2.ru f3.ru"
    ]

noCommandTest :: TestTree
noCommandTest =
  testCase "No command provided" $
    parseArgs [] @?= Left "No command provided. Use 'glados help'."

invalidCommandTest :: TestTree
invalidCommandTest =
  testCase "Invalid command" $
    parseArgs ["invalid"] @?= Left "Invalid command: invalid. Use 'glados help'."

--
-- action tests
--

showActionTest :: TestTree
showActionTest = testCase "Show instance" $ do
  v1 <- evaluate (show ShowUsage)
  v1 @?= "ShowUsage"
  v2 <- evaluate (show (Compile "input.hs" Nothing))
  v2 @?= "Compile \"input.hs\" Nothing"
  v3 <- evaluate (show (Compile "input.hs" (Just "output.o")))
  v3 @?= "Compile \"input.hs\" (Just \"output.o\")"
  v4 <- evaluate (show (Interpret "script.hs"))
  v4 @?= "Interpret \"script.hs\""

eqActionTest :: TestTree
eqActionTest = testCase "Eq instance" $ do
  ShowUsage @?= ShowUsage
  Compile "a.hs" Nothing @?= Compile "a.hs" Nothing
  Interpret "b.hs" @?= Interpret "b.hs"
  assertBool "ShowUsage /= Compile" (ShowUsage /= Compile "a.hs" Nothing)
  assertBool
    "Compile with different output /= Compile with Nothing"
    (Compile "a.hs" (Just "out.o") /= Compile "a.hs" Nothing)
