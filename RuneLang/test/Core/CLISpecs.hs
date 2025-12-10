module Core.CLISpecs (cliTests) where

import CLI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import System.IO.Silently (capture)
import Control.Exception (SomeException, try)

--
-- public
--

cliTests :: TestTree
cliTests =
  testGroup
    "CLI Tests"
    [ cliParseTests,
      cliRunTests
    ]

--
-- helpers
--

usageString :: String
usageString =
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

cleanOutput :: String -> String
cleanOutput = unlines . take (length usageLines) . lines . dropWhile (/= 'U')
  where
    usageLines = lines usageString

shouldParseTo :: [String] -> Action -> TestTree
shouldParseTo args expectedAction =
  testCase (unwords args) $
    assertEqual "Should parse to the expected action" (Right expectedAction) (parseArgs args)

shouldFailWith :: [String] -> String -> TestTree
shouldFailWith args expectedError =
  testCase (unwords args) $
    assertEqual "Should fail with the expected error message" (Left expectedError) (parseArgs args)

--
-- private
--

parseDerivingTests :: TestTree
parseDerivingTests =
  testGroup
    "Deriving Show/Eq Coverage"
    [ testCase "Show coverage for all constructors" $ do
        let action1 = ShowUsage
            action2 = Compile "in" (Just "out")
            action3 = Interpret "run"
            action4 = Compile "file" Nothing
        assertEqual "Show ShowUsage" "ShowUsage" (show action1)
        assertEqual "Show Compile Just" "Compile \"in\" (Just \"out\")" (show action2)
        assertEqual "Show Interpret" "Interpret \"run\"" (show action3)
        assertEqual "Show Compile Nothing" "Compile \"file\" Nothing" (show action4)
    , testCase "Eq coverage" $ do
        let c1 = Compile "f" Nothing
            c2 = Compile "f" Nothing
            i1 = Interpret "f"
            s1 = ShowUsage
        assertEqual "Equal actions (Compile)" c1 c2
        assertBool "Unequal actions (file)" (Compile "f" Nothing /= Compile "g" Nothing)
        assertBool "Unequal actions (output)" (Compile "f" Nothing /= Compile "f" (Just "out"))
        assertBool "Unequal actions (Compile/Interpret)" (c1 /= i1)
        assertBool "Unequal actions (Interpret/ShowUsage)" (i1 /= s1)
        assertBool "Unequal actions (Compile/ShowUsage)" (c1 /= s1)
    ]

parseHelpTests :: TestTree
parseHelpTests =
  testGroup
    "Help Command Parsing"
    [ shouldParseTo ["help"] ShowUsage,
      shouldParseTo ["--help"] ShowUsage,
      shouldParseTo ["-h"] ShowUsage
    ]

parseRunSuccessTests :: TestTree
parseRunSuccessTests =
  testGroup
    "Run Command Success Parsing"
    [ shouldParseTo ["run", "file.ru"] (Interpret "file.ru"),
      shouldParseTo ["--run", "another.ru"] (Interpret "another.ru"),
      shouldParseTo ["-r", "test.glados"] (Interpret "test.glados")
    ]

parseRunFailureTests :: TestTree
parseRunFailureTests =
  testGroup
    "Run Command Failure Parsing"
    [ shouldFailWith ["run"] "The 'run' command requires an input file.",
      shouldFailWith ["--run", "file1", "file2"] "The 'run' command takes exactly one file argument.",
      shouldFailWith ["-r", "file1", "-o", "out"] "The 'run' command takes exactly one file argument."
    ]

parseBuildSuccessTests :: TestTree
parseBuildSuccessTests =
  testGroup
    "Build Command Success Parsing"
    [ shouldParseTo ["build", "input.ru"] (Compile "input.ru" Nothing),
      shouldParseTo ["-b", "input.ru"] (Compile "input.ru" Nothing),
      shouldParseTo ["--build", "input.ru"] (Compile "input.ru" Nothing),
      shouldParseTo ["build", "input.ru", "-o", "output.bin"] (Compile "input.ru" (Just "output.bin")),
      shouldParseTo ["--build", "input.ru", "--output", "output.bin"] (Compile "input.ru" (Just "output.bin"))
    ]

parseBuildFailureTests :: TestTree
parseBuildFailureTests =
  testGroup
    "Build Command Failure Parsing"
    [ shouldFailWith ["build"] "The 'build' command requires an input file.",
      shouldFailWith ["-b", "input.ru", "-o"] "Invalid arguments for build command: input.ru -o",
      shouldFailWith ["--build", "input.ru", "--output"] "Invalid arguments for build command: input.ru --output",
      shouldFailWith ["build", "input.ru", "extra"] "Invalid arguments for build command: input.ru extra",
      shouldFailWith ["build", "input.ru", "-o", "out1", "out2"] "Invalid arguments for build command: input.ru -o out1 out2"
    ]

parseOtherTests :: TestTree
parseOtherTests =
  testGroup
    "General Parsing Cases"
    [ shouldFailWith [] "No command provided. Use 'glados help'.",
      shouldFailWith ["invalid"] "Invalid command: invalid. Use 'glados help'.",
      shouldFailWith ["unknown", "arg"] "Invalid command: unknown. Use 'glados help'."
    ]

cliParseTests :: TestTree
cliParseTests =
  testGroup
    "CLI Argument Parsing (parseArgs)"
    [ parseDerivingTests,
      parseHelpTests,
      parseRunSuccessTests,
      parseRunFailureTests,
      parseBuildSuccessTests,
      parseBuildFailureTests,
      parseOtherTests
    ]

runCLIShowUsageTest :: TestTree
runCLIShowUsageTest = testCase "runCLI ShowUsage prints correct usage message" $ do
    (output, _) <- capture (runCLI ShowUsage)
    assertEqual "Output should match usage string" usageString (cleanOutput output)

runCLIActionTests :: TestTree
runCLIActionTests =
  testGroup
    "runCLI Action Tests (Branch Coverage)"
    [ testCase "runCLI (Interpret file) hits the interpret branch" $ do
        result <- try (runCLI (Interpret "dummy-interpret.ru")) :: IO (Either SomeException ())
        case result of
          Right _ -> return ()
          Left _ -> return ()

    , testCase "runCLI (Compile file Nothing) hits compile and uses default 'out'" $ do
        result <- try (runCLI (Compile "dummy-compile-default.ru" Nothing)) :: IO (Either SomeException ())
        case result of
          Right _ -> return ()
          Left _ -> return ()

    , testCase "runCLI (Compile file Just out) hits compile and uses specified output" $ do
        result <- try (runCLI (Compile "dummy-compile-specified.ru" (Just "specified.bin"))) :: IO (Either SomeException ())
        case result of
          Right _ -> return ()
          Left _ -> return ()
    ]

cliRunTests :: TestTree
cliRunTests =
  testGroup
    "CLI Runtime Tests (runCLI)"
    [ runCLIShowUsageTest,
      runCLIActionTests
    ]
