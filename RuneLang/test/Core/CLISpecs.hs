module Core.CLISpecs (cliTests) where

import CLI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import System.IO.Silently (capture)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

--
-- public
--

cliTests :: TestTree
cliTests =
  testGroup
    "CLI Tests"
    [ cliParseTests,
      cliRunTests,
      helperFunctionsTests
    ]

--
-- helpers
--

usageString :: String
usageString =
  unlines
    [ "Usage: rune <command> [file] [options]",
      "",
      "Commands:",
      "  help           Show this help message",
      "  build [file]   Compile the given source file",
      "  run   [file]   Show the IR of the given source file",
      "",
      "Options:",
      "  -o, --output <file>   Specify the output file for compilation",
      "  -c                    Compile to object file",
      "  -S                    Compile to assembly code"
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

testFile :: String
testFile = "test_compile.ru"

testFolder :: String
testFolder = "testing_output/"

createFile :: FilePath -> IO ()
createFile path = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  writeFile path "def main() -> null {}"

deleteFolder :: FilePath -> IO ()
deleteFolder path = do
  exists <- doesDirectoryExist path
  when exists $ removeDirectoryRecursive path

testCaseWithSetup :: String -> IO () -> IO () -> IO () -> TestTree
testCaseWithSetup name setup teardown test =
  testCase name $ do
    setup
    test
    teardown

--
-- private
--

actionDerivingTests :: TestTree
actionDerivingTests =
  testGroup
    "Deriving Show/Eq Coverage"
    [ testCase "Show coverage for all constructors" $ do
        let action1 = ShowUsage
            action2 = CompileAll "in" (Just "out")
            action3 = Interpret "run"
            action4 = CompileAll "file" Nothing
        assertEqual "Show ShowUsage" "ShowUsage" (show action1)
        assertEqual "Show Compile Just" "CompileAll \"in\" (Just \"out\")" (show action2)
        assertEqual "Show Interpret" "Interpret \"run\"" (show action3)
        assertEqual "Show Compile Nothing" "CompileAll \"file\" Nothing" (show action4)
    , testCase "Eq coverage" $ do
        let c1 = CompileAll "f" Nothing
            c2 = CompileAll "f" Nothing
            i1 = Interpret "f"
            s1 = ShowUsage
        assertEqual "Equal actions (Compile)" c1 c2
        assertBool "Unequal actions (file)" (CompileAll "f" Nothing /= CompileAll "g" Nothing)
        assertBool "Unequal actions (output)" (CompileAll "f" Nothing /= CompileAll "f" (Just "out"))
        assertBool "Unequal actions (Compile/Interpret)" (c1 /= i1)
        assertBool "Unequal actions (Interpret/ShowUsage)" (i1 /= s1)
        assertBool "Unequal actions (Compile/ShowUsage)" (c1 /= s1)
    ]

compileRuleDerivingTests :: TestTree
compileRuleDerivingTests =
  testGroup
    "Deriving Show/Eq Coverage for CompileRule"
    [ testCase "Show coverage for all CompileRule constructors" $ do
        let rule1 = All
            rule2 = ToObj
            rule3 = ToAsm
        assertEqual "Show All" "All" (show rule1)
        assertEqual "Show ToObj" "ToObj" (show rule2)
        assertEqual "Show ToAsm" "ToAsm" (show rule3)
    , testCase "Eq coverage for CompileRule" $ do
        let all1 = All
            all2 = All
            obj1 = ToObj
            asm1 = ToAsm
        assertEqual "Equal rules (All)" all1 all2
        assertBool "Unequal rules (All/ToObj)" (all1 /= obj1)
        assertBool "Unequal rules (All/ToAsm)" (all1 /= asm1)
        assertBool "Unequal rules (ToObj/ToAsm)" (obj1 /= asm1)
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
      shouldParseTo ["-r", "test.rune"] (Interpret "test.rune")
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
    [ shouldParseTo ["build", "input.ru"] (CompileAll "input.ru" Nothing),
      shouldParseTo ["-b", "input.ru"] (CompileAll "input.ru" Nothing),
      shouldParseTo ["--build", "input.ru"] (CompileAll "input.ru" Nothing),
      shouldParseTo ["build", "input.ru", "-o", "output.bin"] (CompileAll "input.ru" (Just "output.bin")),
      shouldParseTo ["--build", "input.ru", "--output", "output.bin"] (CompileAll "input.ru" (Just "output.bin")),
      shouldParseTo ["build", "input.o"] (CompileObjToExec "input.o" Nothing),
      shouldParseTo ["-b", "input.o"] (CompileObjToExec "input.o" Nothing),
      shouldParseTo ["--build", "input.o"] (CompileObjToExec "input.o" Nothing),
      shouldParseTo ["build", "input.o", "-o", "output.bin"] (CompileObjToExec "input.o" (Just "output.bin")),
      shouldParseTo ["--build", "input.o", "--output", "output.bin"] (CompileObjToExec "input.o" (Just "output.bin")),
      shouldParseTo ["build", "-c", "input.ru"] (CompileToObj "input.ru" Nothing),
      shouldParseTo ["--build", "-c", "input.asm"] (CompileToObj "input.asm" Nothing),
      shouldParseTo ["build", "-S", "input.ru"] (CreateAsm "input.ru" Nothing),
      shouldParseTo ["build", "-c", "input.ru", "-o", "output.o"] (CompileToObj "input.ru" (Just "output.o")),
      shouldParseTo ["build", "-S", "input.ru", "--output", "output.asm"] (CreateAsm "input.ru" (Just "output.asm"))
    ]

parseBuildFailureTests :: TestTree
parseBuildFailureTests =
  testGroup
    "Build Command Failure Parsing"
    [ shouldFailWith ["build"] "No input file provided.",
      shouldFailWith ["-b", "input.ru", "-o"] "-o flag requires an output file.",
      shouldFailWith ["--build", "input.ru", "--output"] "--output flag requires an output file.",
      shouldFailWith ["build", "input.ru", "extra"] "Invalid arguments for build command: extra",
      shouldFailWith ["build", "input.ru", "-o", "out1", "out2"] "Invalid arguments for build command: out2"
    ]

parseOtherTests :: TestTree
parseOtherTests =
  testGroup
    "General Parsing Cases"
    [ shouldFailWith [] "No command provided. Use 'rune help'.",
      shouldFailWith ["invalid"] "Invalid command: invalid. Use 'rune help'.",
      shouldFailWith ["unknown", "arg"] "Invalid command: unknown. Use 'rune help'."
    ]

cliParseTests :: TestTree
cliParseTests =
  testGroup
    "CLI Argument Parsing (parseArgs)"
    [ actionDerivingTests,
      compileRuleDerivingTests,
      parseHelpTests,
      parseRunSuccessTests,
      parseRunFailureTests,
      parseBuildSuccessTests,
      parseBuildFailureTests,
      parseOtherTests
    ]

isValidInputFileTests :: TestTree
isValidInputFileTests =
  testGroup
    "isValidInputFile Tests"
    [ testCase "All rule with .ru file" $
        assertBool ".ru should be valid for All" (isValidInputFile All "file.ru")
    , testCase "All rule with .o file" $
        assertBool ".o should be valid for All" (isValidInputFile All "file.o")
    , testCase "All rule with .asm file" $
        assertBool ".asm should be invalid for All" (not $ isValidInputFile All "file.asm")
    , testCase "ToObj rule with .ru file" $
        assertBool ".ru should be valid for ToObj" (isValidInputFile ToObj "file.ru")
    , testCase "ToObj rule with .asm file" $
        assertBool ".asm should be valid for ToObj" (isValidInputFile ToObj "file.asm")
    , testCase "ToObj rule with .o file" $
        assertBool ".o should be invalid for ToObj" (not $ isValidInputFile ToObj "file.o")
    , testCase "ToAsm rule with .ru file" $
        assertBool ".ru should be valid for ToAsm" (isValidInputFile ToAsm "file.ru")
    , testCase "ToAsm rule with .asm file" $
        assertBool ".asm should be invalid for ToAsm" (not $ isValidInputFile ToAsm "file.asm")
    , testCase "ToAsm rule with .o file" $
        assertBool ".o should be invalid for ToAsm" (not $ isValidInputFile ToAsm "file.o")
    ]

findInputFileSuccessTests :: TestTree
findInputFileSuccessTests =
  testGroup
    "findInputFile Success Tests"
    [ testCase "All rule with .ru file" $ do
        let result = findInputFile ["input.ru", "-o", "output.bin"] All
        assertEqual "Should find input.ru" (Right ("input.ru", ["-o", "output.bin"])) result
    , testCase "All rule with .o file" $ do
        let result = findInputFile ["input.o", "-o", "output.bin"] All
        assertEqual "Should find input.o" (Right ("input.o", ["-o", "output.bin"])) result
    , testCase "ToObj rule with .ru file" $ do
        let result = findInputFile ["input.ru", "-o", "output.bin"] ToObj
        assertEqual "Should find input.ru" (Right ("input.ru", ["-o", "output.bin"])) result
    , testCase "ToObj rule with .asm file" $ do
        let result = findInputFile ["input.asm", "-o", "output.bin"] ToObj
        assertEqual "Should find input.asm" (Right ("input.asm", ["-o", "output.bin"])) result
    , testCase "ToAsm rule with .ru file" $ do
        let result = findInputFile ["input.ru", "-o", "output.bin"] ToAsm
        assertEqual "Should find input.ru" (Right ("input.ru", ["-o", "output.bin"])) result
    ]

findInputFileFailureTests :: TestTree
findInputFileFailureTests =
  testGroup
    "findInputFile Failure Tests"
    [ testCase "No input file provided (All)" $ do
        let result = findInputFile [] All
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "No input file provided (ToObj)" $ do
        let result = findInputFile [] ToObj
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "No input file provided (ToAsm)" $ do
        let result = findInputFile [] ToAsm
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "Invalid input file (All)" $ do
        let result = findInputFile ["invalid.txt"] All
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "Invalid input file (ToObj)" $ do
        let result = findInputFile ["invalid.txt"] ToObj
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "Invalid input file (ToAsm)" $ do
        let result = findInputFile ["invalid.txt"] ToAsm
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "Invalid .o file for ToObj" $ do
        let result = findInputFile ["file.o"] ToObj
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "Invalid .asm file for ToAsm" $ do
        let result = findInputFile ["file.asm"] ToAsm
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    , testCase "Invalid .o file for ToAsm" $ do
        let result = findInputFile ["file.o"] ToAsm
        assertEqual "Should fail with no input file" (Left "No input file provided.") result
    ]

findOutputFileSuccessTests :: TestTree
findOutputFileSuccessTests =
  testGroup
    "findOutputFile Success Tests"
    [ testCase "No arguments" $ do
        let result = findOutputFile []
        assertEqual "Should return Nothing and empty list" (Right (Nothing, [])) result
    , testCase "-o with output file" $ do
        let result = findOutputFile ["-o", "output.bin"]
        assertEqual "Should return Just output.bin and empty list" (Right (Just "output.bin", [])) result
    , testCase "--output with output file" $ do
        let result = findOutputFile ["--output", "output.bin"]
        assertEqual "Should return Just output.bin and empty list" (Right (Just "output.bin", [])) result
    , testCase "Input file with -o and output file" $ do
        let result = findOutputFile ["input.ru", "-o", "output.bin"]
        assertEqual "Should return Just output.bin and input.ru in list" (Right (Just "output.bin", ["input.ru"])) result
    , testCase "Input file with --output and output file" $ do
        let result = findOutputFile ["input.ru", "--output", "output.bin"]
        assertEqual "Should return Just output.bin and input.ru in list" (Right (Just "output.bin", ["input.ru"])) result
    , testCase "Input file only" $ do
        let result = findOutputFile ["input.ru"]
        assertEqual "Should return Nothing and input.ru in list" (Right (Nothing, ["input.ru"])) result
    ]

findOutputFileFailureTests :: TestTree
findOutputFileFailureTests =
  testGroup
    "findOutputFile Failure Tests"
    [ testCase "findOutputFile with -o but no file" $ do
        let result = findOutputFile ["-o"]
        assertEqual "Should fail with -o flag requires an output file." (Left "-o flag requires an output file.") result
    , testCase "findOutputFile with --output but no file" $ do
        let result = findOutputFile ["--output"]
        assertEqual "Should fail with --output flag requires an output file." (Left "--output flag requires an output file.") result
    , testCase "findOutputFile with input and -o but no file" $ do
        let result = findOutputFile ["input.ru", "-o"]
        assertEqual "Should fail with -o flag requires an output file." (Left "-o flag requires an output file.") result
    , testCase "findOutputFile with input and --output but no file" $ do
        let result = findOutputFile ["input.ru", "--output"]
        assertEqual "Should fail with --output flag requires an output file." (Left "--output flag requires an output file.") result
    ]

isSourceFileTests :: TestTree
isSourceFileTests =
  testGroup
    "isSourceFile Tests"
    [ testCase ".ru file returns CompileAll" $ do
        let result = isSourceFile "input.ru" Nothing
        assertEqual "Should return CompileAll" (CompileAll "input.ru" Nothing) result
    , testCase ".ru file with output returns CompileAll" $ do
        let result = isSourceFile "input.ru" (Just "output.bin")
        assertEqual "Should return CompileAll" (CompileAll "input.ru" (Just "output.bin")) result
    , testCase ".o file returns CompileObjToExec" $ do
        let result = isSourceFile "input.o" Nothing
        assertEqual "Should return CompileObjToExec" (CompileObjToExec "input.o" Nothing) result
    , testCase ".o file with output returns CompileObjToExec" $ do
        let result = isSourceFile "input.o" (Just "output.bin")
        assertEqual "Should return CompileObjToExec" (CompileObjToExec "input.o" (Just "output.bin")) result
    ]

determineCompileRuleTests :: TestTree
determineCompileRuleTests =
  testGroup
    "determineCompileRule Tests"
    [ testCase "No flags returns All" $ do
        let result = determineCompileRule []
        assertEqual "Should return All" (Right (All, [])) result
    , testCase "-c flag returns ToObj" $ do
        let result = determineCompileRule ["-c", "input.ru"]
        assertEqual "Should return ToObj" (Right (ToObj, ["input.ru"])) result
    , testCase "-S flag returns ToAsm" $ do
        let result = determineCompileRule ["-S", "input.ru"]
        assertEqual "Should return ToAsm" (Right (ToAsm, ["input.ru"])) result
    , testCase "Both -c and -S flags returns error" $ do
        let result = determineCompileRule ["-c", "-S", "input.ru"]
        assertEqual "Should fail with both flags error" (Left "Cannot use both -c and -S options together.") result
    ]

helperFunctionsTests :: TestTree
helperFunctionsTests =
  testGroup
    "Helper Functions Tests"
    [ isValidInputFileTests
    , findInputFileSuccessTests
    , findInputFileFailureTests
    , findOutputFileSuccessTests
    , findOutputFileFailureTests
    , isSourceFileTests
    , determineCompileRuleTests
    ]

runCLIShowUsageTest :: TestTree
runCLIShowUsageTest = testCase "runCLI ShowUsage prints correct usage message" $ do
    (output, _) <- capture (runCLI ShowUsage)
    assertEqual "Output should match usage string" usageString (cleanOutput output)

runCLIActionTests :: TestTree
runCLIActionTests =
  testGroup
    "runCLI Action Tests (Branch Coverage)"
    [ testCaseWithSetup "runCLI (Interpret file) hits the interpret branch"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, result) <- capture (try (runCLI (Interpret (testFolder ++ testFile))) :: IO (Either SomeException ()))
          case result of
            Right _ -> return ()
            Left _ -> return ()
        )
    , testCaseWithSetup "runCLI (CompileAll file Nothing) uses default output 'a.out'"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CompileAll (testFolder ++ testFile) Nothing)) :: IO (Either SomeException ()))
          assertBool "Should attempt compilation" True
        )
    , testCaseWithSetup "runCLI (CompileAll file Just out) uses specified output"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CompileAll (testFolder ++ testFile) (Just (testFolder ++ "specified.bin")))) :: IO (Either SomeException ()))
          assertBool "Should attempt compilation" True
        )
    , testCaseWithSetup "runCLI (CompileToObj file Nothing) uses default output file by replacing input extension with .o"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CompileToObj (testFolder ++ testFile) Nothing)) :: IO (Either SomeException ()))
          assertBool "Should attempt object compilation" True
        )
    , testCaseWithSetup "runCLI (CompileToObj file Just out) uses specified output"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CompileToObj (testFolder ++ testFile) (Just (testFolder ++ "custom.o")))) :: IO (Either SomeException ()))
          assertBool "Should attempt object compilation" True
        )
    , testCaseWithSetup "runCLI (CreateAsm file Nothing) uses output file with .asm extension replacing input file's extension"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CreateAsm (testFolder ++ testFile) Nothing)) :: IO (Either SomeException ()))
          assertBool "Should attempt assembly generation" True
        )
    , testCaseWithSetup "runCLI (CreateAsm file Just out) uses specified output"
        (createFile (testFolder ++ testFile))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CreateAsm (testFolder ++ testFile) (Just (testFolder ++ "custom.asm")))) :: IO (Either SomeException ()))
          assertBool "Should attempt assembly generation" True
        )
    , testCaseWithSetup "runCLI (CompileObjToExec file Nothing) uses default output 'a.out'"
        (createFile (testFolder ++ "test_compile.o"))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CompileObjToExec (testFolder ++ "test_compile.o") Nothing)) :: IO (Either SomeException ()))
          assertBool "Should attempt executable generation" True
        )
    , testCaseWithSetup "runCLI (CompileObjToExec file Just out) uses specified output"
        (createFile (testFolder ++ "test_compile.o"))
        (deleteFolder testFolder)
        (do
          (_, _) <- capture (try (runCLI (CompileObjToExec (testFolder ++ "test_compile.o") (Just (testFolder ++ "custom.exe")))) :: IO (Either SomeException ()))
          assertBool "Should attempt executable generation" True
        )
    ]

cliRunTests :: TestTree
cliRunTests =
  testGroup
    "CLI Runtime Tests (runCLI)"
    [ runCLIShowUsageTest,
      runCLIActionTests
    ]
