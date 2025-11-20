module CLISpec (cliTests) where

import CLI (Action (..), parseArgs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

cliTests :: TestTree
cliTests =
  testGroup
    "CLI Argument Parsing Tests"
    [ parseArgsTests
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
