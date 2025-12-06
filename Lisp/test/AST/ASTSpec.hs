{-# LANGUAGE OverloadedStrings #-}

module AST.ASTSpec (astTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Lisp.AST.AST

astTests :: TestTree
astTests = testGroup "AST Tests"
  [ compEnvTests
  , extractIntegerTests
  , operatorTests
  , conditionalTests
  , definitionTests
  , lambdaTests
  , sequenceTests
  , multipleExpressionsTests
  , errorHandlingTests
  , listTests
  , symbolTests
  ]

compEnvTests :: TestTree
compEnvTests = testGroup "compEnv"
  [ testCase "finds variable" $ compEnv [("x", AstInteger 5)] "x" @?= Just (AstInteger 5)
  , testCase "returns Nothing when missing" $ compEnv [("x", AstInteger 5)] "z" @?= Nothing
  , testCase "handles empty environment" $ compEnv [] "x" @?= Nothing
  ]

extractIntegerTests :: TestTree
extractIntegerTests = testGroup "extractInteger"
  [ testCase "extracts from AstInteger" $ extractInteger [] (AstInteger 42) @?= Just 42
  , testCase "extracts from environment variable" $
      extractInteger [("x", AstInteger 5)] (AstSymbol "x") @?= Just 5
  , testCase "returns Nothing for non-integer" $
      extractInteger [] (AstBoolean True) @?= Nothing
  ]

operatorTests :: TestTree
operatorTests = testGroup "Operators"
  [ testCase "+ evaluates correctly" $
      case evalAST [] (Call "+" [AstInteger 3, AstInteger 4]) of
        (_, Right (AstInteger 7)) -> return ()
        _ -> assertFailure "Expected 7"
  , testCase "- evaluates correctly" $
      case evalAST [] (Call "-" [AstInteger 10, AstInteger 3]) of
        (_, Right (AstInteger 7)) -> return ()
        _ -> assertFailure "Expected 7"
  , testCase "* evaluates correctly" $
      case evalAST [] (Call "*" [AstInteger 3, AstInteger 4]) of
        (_, Right (AstInteger 12)) -> return ()
        _ -> assertFailure "Expected 12"
  , testCase "div evaluates correctly" $
      case evalAST [] (Call "div" [AstInteger 10, AstInteger 2]) of
        (_, Right (AstInteger 5)) -> return ()
        _ -> assertFailure "Expected 5"
  , testCase "mod evaluates correctly" $
      case evalAST [] (Call "mod" [AstInteger 10, AstInteger 3]) of
        (_, Right (AstInteger 1)) -> return ()
        _ -> assertFailure "Expected 1"
  , testCase "eq? returns true" $
      case evalAST [] (Call "eq?" [AstInteger 5, AstInteger 5]) of
        (_, Right (AstBoolean True)) -> return ()
        _ -> assertFailure "Expected #t"
  , testCase "eq? returns false" $
      case evalAST [] (Call "eq?" [AstInteger 5, AstInteger 3]) of
        (_, Right (AstBoolean False)) -> return ()
        _ -> assertFailure "Expected #f"
  , testCase "< returns true" $
      case evalAST [] (Call "<" [AstInteger 3, AstInteger 5]) of
        (_, Right (AstBoolean True)) -> return ()
        _ -> assertFailure "Expected #t"
  , testCase "< returns false" $
      case evalAST [] (Call "<" [AstInteger 5, AstInteger 3]) of
        (_, Right (AstBoolean False)) -> return ()
        _ -> assertFailure "Expected #f"
  , testCase "div by zero fails" $
      case evalAST [] (Call "div" [AstInteger 10, AstInteger 0]) of
        (_, Left _) -> return ()
        _ -> assertFailure "Should fail on division by zero"
  , testCase "mod by zero fails" $
      case evalAST [] (Call "mod" [AstInteger 10, AstInteger 0]) of
        (_, Left _) -> return ()
        _ -> assertFailure "Should fail on modulo by zero"
  ]

conditionalTests :: TestTree
conditionalTests = testGroup "If expressions"
  [ testCase "true branch evaluates" $
      evalAST [] (If (AstBoolean True) (AstInteger 1) (AstInteger 0)) @?=
        ([], Right (AstInteger 1))
  , testCase "false branch evaluates" $
      evalAST [] (If (AstBoolean False) (AstInteger 1) (AstInteger 0)) @?=
        ([], Right (AstInteger 0))
  , testCase "rejects non-boolean condition" $
      case evalAST [] (If (AstInteger 5) (AstInteger 1) (AstInteger 0)) of
        (_, Left _) -> return ()
        _ -> assertFailure "Should reject non-boolean"
  , testCase "evaluates nested condition" $
      case evalAST [] (If (Call "<" [AstInteger 3, AstInteger 5]) (AstInteger 1) (AstInteger 0)) of
        (_, Right (AstInteger 1)) -> return ()
        _ -> assertFailure "Should evaluate nested condition"
  ]

definitionTests :: TestTree
definitionTests = testGroup "Define"
  [ testCase "defines variable" $ do
      let (env, result) = evalAST [] (Define "x" (AstInteger 5))
      result @?= Right (AstSymbol "")
      compEnv env "x" @?= Just (AstInteger 5)
  , testCase "redefines variable" $ do
      let (env, _) = evalAST [("x", AstInteger 5)] (Define "x" (AstInteger 10))
      compEnv env "x" @?= Just (AstInteger 10)
  , testCase "defines with evaluated expression" $ do
      let (env, _) = evalAST [("y", AstInteger 5)] (Define "x" (AstSymbol "y"))
      compEnv env "x" @?= Just (AstInteger 5)
  ]

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda"
  [ testCase "captures environment" $
      case evalAST [("y", AstInteger 10)] (Lambda ["x"] (AstSymbol "y") []) of
        (_, Right (Lambda _ _ closure)) -> length closure @?= 1
        _ -> assertFailure "Should capture environment"
  , testCase "calls with arguments" $ do
      let lambda = Lambda ["x"] (Call "+" [AstSymbol "x", AstInteger 10]) []
      case evalAST [("f", lambda)] (AstList [AstSymbol "f", AstInteger 5]) of
        (_, Right (AstInteger 15)) -> return ()
        _ -> assertFailure "Should call lambda with arguments"
  , testCase "rejects wrong argument count" $ do
      let lambda = Lambda ["x", "y"] (AstInteger 1) []
      case evalAST [("f", lambda)] (AstList [AstSymbol "f", AstInteger 5]) of
        (_, Left _) -> return ()
        _ -> assertFailure "Should reject wrong argument count"
  ]

sequenceTests :: TestTree
sequenceTests = testGroup "Sequences"
  [ testCase "evaluates single expression" $
      case evalASTWithEnv [] [AstInteger 5] of
        (_, Right [AstInteger 5]) -> return ()
        _ -> assertFailure "Should evaluate single expression"
  , testCase "evaluates multiple expressions" $
      case evalASTWithEnv [] [AstInteger 1, AstInteger 2, AstInteger 3] of
        (_, Right [AstInteger 1, AstInteger 2, AstInteger 3]) -> return ()
        _ -> assertFailure "Should evaluate all expressions"
  , testCase "chains expressions with define" $
      case evalASTWithEnv [] [Define "x" (AstInteger 5), AstSymbol "x"] of
        (_, Right [AstSymbol "", AstInteger 5]) -> return ()
        _ -> assertFailure "Should chain expressions"
  , testCase "stops on error" $
      case evalASTWithEnv [] [AstInteger 1, AstSymbol "undefined"] of
        (_, Left _) -> return ()
        _ -> assertFailure "Should stop on error"
  , testCase "propagates environment" $
      case evalASTWithEnv [] [Define "x" (AstInteger 5), Define "y" (AstInteger 10), AstSymbol "y"] of
        (env, Right [AstSymbol "", AstSymbol "", AstInteger 10]) ->
          length env @?= 2
        _ -> assertFailure "Should propagate environment"
  ]

multipleExpressionsTests :: TestTree
multipleExpressionsTests = testGroup "Multiple Expressions (new feature)"
  [ testCase "evaluates two arithmetic operations" $
      case evalASTWithEnv [] [Call "-" [AstInteger 4, AstInteger 5], Call "+" [AstInteger 3, AstInteger 2]] of
        (_, Right [AstInteger (-1), AstInteger 5]) -> return ()
        _ -> assertFailure "Expected [-1, 5]"

  , testCase "evaluates three operations" $
      case evalASTWithEnv []
        [ Call "*" [AstInteger 2, AstInteger 3]
        , Call "-" [AstInteger 10, AstInteger 7]
        , Call "+" [AstInteger 1, AstInteger 1]
        ] of
        (_, Right [AstInteger 6, AstInteger 3, AstInteger 2]) -> return ()
        _ -> assertFailure "Expected [6, 3, 2]"

  , testCase "mixes operations and literals" $
      case evalASTWithEnv []
        [ AstInteger 42
        , Call "+" [AstInteger 10, AstInteger 5]
        , AstBoolean True
        ] of
        (_, Right [AstInteger 42, AstInteger 15, AstBoolean True]) -> return ()
        _ -> assertFailure "Expected [42, 15, #t]"

  , testCase "evaluates with variable definitions between expressions" $
      case evalASTWithEnv []
        [ Call "+" [AstInteger 1, AstInteger 2]
        , Define "x" (AstInteger 10)
        , Call "*" [AstSymbol "x", AstInteger 2]
        ] of
        (_, Right [AstInteger 3, AstSymbol "", AstInteger 20]) -> return ()
        _ -> assertFailure "Expected [3, \"\", 20]"

  , testCase "evaluates comparisons in sequence" $
      case evalASTWithEnv []
        [ Call "<" [AstInteger 3, AstInteger 5]
        , Call "eq?" [AstInteger 10, AstInteger 10]
        , Call "<" [AstInteger 5, AstInteger 3]
        ] of
        (_, Right [AstBoolean True, AstBoolean True, AstBoolean False]) -> return ()
        _ -> assertFailure "Expected [#t, #t, #f]"

  , testCase "evaluates if expressions in sequence" $
      case evalASTWithEnv []
        [ If (AstBoolean True) (AstInteger 1) (AstInteger 0)
        , If (AstBoolean False) (AstInteger 1) (AstInteger 0)
        ] of
        (_, Right [AstInteger 1, AstInteger 0]) -> return ()
        _ -> assertFailure "Expected [1, 0]"

  , testCase "stops on first error in sequence" $
      case evalASTWithEnv []
        [ Call "+" [AstInteger 1, AstInteger 2]
        , Call "div" [AstInteger 10, AstInteger 0]  -- Division by zero
        , Call "*" [AstInteger 3, AstInteger 4]
        ] of
        (_, Left _) -> return ()
        _ -> assertFailure "Should stop on division by zero"

  , testCase "complex sequence with defines and uses" $
      case evalASTWithEnv []
        [ Define "a" (AstInteger 5)
        , Define "b" (AstInteger 10)
        , Call "+" [AstSymbol "a", AstSymbol "b"]
        , Call "*" [AstSymbol "a", AstInteger 2]
        ] of
        (_, Right [AstSymbol "", AstSymbol "", AstInteger 15, AstInteger 10]) -> return ()
        _ -> assertFailure "Expected [\"\", \"\", 15, 10]"
  ]

symbolTests :: TestTree
symbolTests = testGroup "Symbols"
  [ testCase "evaluates #t" $
      evalAST [] (AstSymbol "#t") @?= ([], Right (AstBoolean True))
  , testCase "evaluates #f" $
      evalAST [] (AstSymbol "#f") @?= ([], Right (AstBoolean False))
  , testCase "looks up variable" $
      evalAST [("x", AstInteger 10)] (AstSymbol "x") @?=
        ([("x", AstInteger 10)], Right (AstInteger 10))
  , testCase "rejects undefined" $
      case evalAST [] (AstSymbol "undefined") of
        (_, Left _) -> return ()
        _ -> assertFailure "Should reject undefined variable"
  ]

listTests :: TestTree
listTests = testGroup "Lists"
  [ testCase "evaluates empty list" $
      evalAST [] (AstList []) @?= ([], Right (AstList []))
  , testCase "unwraps single element" $
      evalAST [] (AstList [AstInteger 5]) @?= ([], Right (AstInteger 5))
  , testCase "calls operator in list" $
      case evalAST [] (AstList [AstSymbol "+", AstInteger 2, AstInteger 3]) of
        (_, Right (AstInteger 5)) -> return ()
        _ -> assertFailure "Should call operator"
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error handling"
  [ testCase "undefined variable" $
      case evalAST [] (AstSymbol "undefined") of
        (_, Left _) -> return ()
        _ -> assertFailure "Should reject undefined variable"
  , testCase "empty list evaluation" $
      case evalASTWithEnv [] [] of
        (_, Right []) -> return ()
        _ -> assertFailure "Should return empty list"
  , testCase "invalid list" $
      case evalAST [] (AstList [AstInteger 1, AstInteger 2]) of
        (_, Left _) -> return ()
        _ -> assertFailure "Should reject invalid list"
  , testCase "non-integer argument" $
      case evalAST [] (Call "+" [AstBoolean True, AstInteger 2]) of
        (_, Left _) -> return ()
        _ -> assertFailure "Should reject non-integer argument"
  ]
