{-# LANGUAGE OverloadedStrings #-}

module AST.SExprToASTSpec (sexprToASTTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Lisp.AST.AST
import Lisp.SExpr.SExpr
import Lisp.AST.SExprToAST (
    sexprToAST,
    extractSymbol,
    extractParams,
    parseDefineFunction,
    parseDefineVariable,
    parseLambda,
    parseIf,
    parseFunctionCall
  )

sexprToASTTests :: TestTree
sexprToASTTests = testGroup "sexprToAST"
  [ integerConversionTests
  , symbolConversionTests
  , defineExpressionTests
  , lambdaExpressionTests
  , ifExpressionTests
  , functionCallTests
  , listConversionTests
  , errorHandlingTests
  , extractSymbolTests
  , extractParamsTests
  , parseDefineFunctionTests
  , parseDefineVariableTests
  , parseLambdaTests
  , parseIfTests
  , parseFunctionCallTests
  ]

integerConversionTests :: TestTree
integerConversionTests = testGroup "Integer conversion"
  [ testCase "converts positive integer" $ do
      sexprToAST (Integer 42) @?= Right (AstInteger 42)

  , testCase "converts negative integer" $ do
      sexprToAST (Integer (-5)) @?= Right (AstInteger (-5))

  , testCase "converts zero" $ do
      sexprToAST (Integer 0) @?= Right (AstInteger 0)
  ]

symbolConversionTests :: TestTree
symbolConversionTests = testGroup "Symbol conversion"
  [ testCase "converts simple symbol" $ do
      sexprToAST (Symbol "foo") @?= Right (AstSymbol "foo")

  , testCase "converts operator symbol" $ do
      sexprToAST (Symbol "+") @?= Right (AstSymbol "+")

  , testCase "converts custom symbol" $ do
      sexprToAST (Symbol "myVar") @?= Right (AstSymbol "myVar")

  , testCase "rejects 'define' keyword alone" $ do
      case sexprToAST (Symbol "define") of
        Left _ -> return ()
        Right _ -> fail "Should reject 'define' symbol"

  , testCase "rejects 'lambda' keyword alone" $ do
      case sexprToAST (Symbol "lambda") of
        Left _ -> return ()
        Right _ -> fail "Should reject 'lambda' symbol"

  , testCase "rejects 'if' keyword alone" $ do
      case sexprToAST (Symbol "if") of
        Left _ -> return ()
        Right _ -> fail "Should reject 'if' symbol"
  ]

defineExpressionTests :: TestTree
defineExpressionTests = testGroup "Define expression"
  [ testCase "converts variable define" $ do
      let sexpr = List [Symbol "define", Symbol "x", Integer 5]
      sexprToAST sexpr @?= Right (Define "x" (AstInteger 5))

  , testCase "converts nested define" $ do
      let sexpr = List [Symbol "define", Symbol "y", List [Symbol "+", Integer 1, Integer 2]]
      case sexprToAST sexpr of
        Right (Define name _) -> name @?= "y"
        _ -> fail "Should convert nested define"

  , testCase "converts function define" $ do
      let sexpr = List [Symbol "define", List [Symbol "add", Symbol "a", Symbol "b"], 
                        List [Symbol "+", Symbol "a", Symbol "b"]]
      case sexprToAST sexpr of
        Right (Define name (Lambda params _ _)) -> do
          name @?= "add"
          length params @?= 2
        _ -> fail "Should convert function define"
  ]

lambdaExpressionTests :: TestTree
lambdaExpressionTests = testGroup "Lambda expression"
  [ testCase "converts simple lambda" $ do
      let sexpr = List [Symbol "lambda", List [Symbol "x"], Integer 1]
      sexprToAST sexpr @?= Right (Lambda ["x"] (AstInteger 1) [])

  , testCase "converts lambda with multiple parameters" $ do
      let sexpr = List [Symbol "lambda", List [Symbol "x", Symbol "y"],
                        List [Symbol "+", Symbol "x", Symbol "y"]]
      case sexprToAST sexpr of
        Right (Lambda params _ _) -> length params @?= 2
        _ -> fail "Should convert lambda with multiple params"

  , testCase "converts lambda with complex body" $ do
      let sexpr = List [Symbol "lambda", List [Symbol "n"],
                        List [Symbol "*", Symbol "n", Integer 2]]
      case sexprToAST sexpr of
        Right (Lambda _ body _) -> body @?= Call "*" [AstSymbol "n", AstInteger 2]
        _ -> fail "Should convert lambda with complex body"
  ]

ifExpressionTests :: TestTree
ifExpressionTests = testGroup "If expression"
  [ testCase "converts if expression with true condition" $ do
      let sexpr = List [Symbol "if", Symbol "#t", Integer 1, Integer 0]
      case sexprToAST sexpr of
        Right (If _ _ _) -> return ()
        _ -> fail "Should convert if expression"

  , testCase "converts if with false condition" $ do
      let sexpr = List [Symbol "if", Symbol "#f", Integer 1, Integer 0]
      case sexprToAST sexpr of
        Right (If cond _ _) ->
          cond @?= AstSymbol "#f"
        _ -> fail "Should convert if with false"

  , testCase "converts if with comparison" $ do
      let sexpr = List [Symbol "if", List [Symbol "<", Integer 5, Integer 10],
                        Integer 1, Integer 0]
      case sexprToAST sexpr of
        Right (If _ _ _) -> return ()
        _ -> fail "Should convert if with comparison"
  ]

functionCallTests :: TestTree
functionCallTests = testGroup "Function call"
  [ testCase "converts addition call" $ do
      let sexpr = List [Symbol "+", Integer 1, Integer 2]
      sexprToAST sexpr @?= Right (Call "+" [AstInteger 1, AstInteger 2])

  , testCase "converts subtraction call" $ do
      let sexpr = List [Symbol "-", Integer 5, Integer 3]
      sexprToAST sexpr @?= Right (Call "-" [AstInteger 5, AstInteger 3])

  , testCase "converts multiplication call" $ do
      let sexpr = List [Symbol "*", Integer 3, Integer 4]
      sexprToAST sexpr @?= Right (Call "*" [AstInteger 3, AstInteger 4])

  , testCase "converts division call" $ do
      let sexpr = List [Symbol "div", Integer 10, Integer 2]
      sexprToAST sexpr @?= Right (Call "div" [AstInteger 10, AstInteger 2])

  , testCase "converts comparison call" $ do
      let sexpr = List [Symbol "<", Integer 5, Integer 10]
      sexprToAST sexpr @?= Right (Call "<" [AstInteger 5, AstInteger 10])

  , testCase "converts nested function call" $ do
      let sexpr = List [Symbol "+", List [Symbol "*", Integer 2, Integer 3], Integer 4]
      case sexprToAST sexpr of
        Right (Call "+" [Call "*" _ , AstInteger 4]) -> return ()
        _ -> fail "Should convert nested call"

  , testCase "converts mod operator" $ do
      let sexpr = List [Symbol "mod", Integer 10, Integer 3]
      sexprToAST sexpr @?= Right (Call "mod" [AstInteger 10, AstInteger 3])

  , testCase "converts eq? operator" $ do
      let sexpr = List [Symbol "eq?", Integer 5, Integer 5]
      sexprToAST sexpr @?= Right (Call "eq?" [AstInteger 5, AstInteger 5])

  , testCase "converts custom function call to list" $ do
      let sexpr = List [Symbol "customFunc", Integer 1, Integer 2]
      sexprToAST sexpr @?= Right (AstList [AstSymbol "customFunc", AstInteger 1, AstInteger 2])
  ]

listConversionTests :: TestTree
listConversionTests = testGroup "List conversion"
  [ testCase "converts simple list" $ do
      let sexpr = List [Integer 1, Integer 2, Integer 3]
      sexprToAST sexpr @?= Right (AstList [AstInteger 1, AstInteger 2, AstInteger 3])

  , testCase "converts list with symbols" $ do
      let sexpr = List [Symbol "a", Symbol "b", Symbol "c"]
      sexprToAST sexpr @?= Right (AstList [AstSymbol "a", AstSymbol "b", AstSymbol "c"])

  , testCase "converts nested lists" $ do
      let sexpr = List [List [Integer 1, Integer 2], Integer 3]
      case sexprToAST sexpr of
        Right (AstList [AstList _, AstInteger 3]) -> return ()
        _ -> fail "Should convert nested lists"
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error handling"
  [ testCase "rejects empty list" $ do
      case sexprToAST (List []) of
        Left _ -> return ()
        Right _ -> fail "Should reject empty list"

  , testCase "rejects malformed define (no arguments)" $ do
      case sexprToAST (List [Symbol "define"]) of
        Left _ -> return ()
        Right _ -> fail "Should reject malformed define"

  , testCase "rejects define with non-symbol name" $ do
      case sexprToAST (List [Symbol "define", Integer 5, Integer 10]) of
        Left _ -> return ()
        Right _ -> fail "Should reject define with non-symbol name"

  , testCase "rejects define with 4 arguments" $ do
      case sexprToAST (List [Symbol "define", Symbol "x", Integer 1, Integer 2]) of
        Left _ -> return ()
        Right _ -> fail "Should reject define with 4+ args"

  , testCase "rejects malformed lambda (missing body)" $ do
      case sexprToAST (List [Symbol "lambda", List [Symbol "x"]]) of
        Left _ -> return ()
        Right _ -> fail "Should reject malformed lambda"

  , testCase "rejects lambda with non-list parameters" $ do
      case sexprToAST (List [Symbol "lambda", Symbol "x", Integer 1]) of
        Left _ -> return ()
        Right _ -> fail "Should reject lambda with non-list params"

  , testCase "rejects lambda with 4 arguments" $ do
      case sexprToAST (List [Symbol "lambda", List [Symbol "x"], Integer 1, Integer 2]) of
        Left _ -> return ()
        Right _ -> fail "Should reject lambda with 4+ args"

  , testCase "rejects malformed if (missing branches)" $ do
      case sexprToAST (List [Symbol "if", Symbol "#t"]) of
        Left _ -> return ()
        Right _ -> fail "Should reject malformed if"

  , testCase "rejects if with only condition" $ do
      case sexprToAST (List [Symbol "if", Symbol "#t", Integer 1]) of
        Left _ -> return ()
        Right _ -> fail "Should reject if with missing else"

  , testCase "rejects if with 5 arguments" $ do
      case sexprToAST (List [Symbol "if", Symbol "#t", Integer 1, Integer 0, Integer 99]) of
        Left _ -> return ()
        Right _ -> fail "Should reject if with 5+ args"
  ]

extractSymbolTests :: TestTree
extractSymbolTests = testGroup "extractSymbol"
  [ testCase "extracts symbol" $ do
      extractSymbol (Symbol "x") @?= Right "x"

  , testCase "rejects integer" $ do
      case extractSymbol (Integer 5) of
        Left _ -> return ()
        Right _ -> fail "Should reject integer"

  , testCase "rejects list" $ do
      case extractSymbol (List []) of
        Left _ -> return ()
        Right _ -> fail "Should reject list"
  ]

extractParamsTests :: TestTree
extractParamsTests = testGroup "extractParams"
  [ testCase "extracts single param" $ do
      extractParams [Symbol "x"] @?= Right ["x"]

  , testCase "extracts multiple params" $ do
      extractParams [Symbol "x", Symbol "y"] @?= Right ["x", "y"]

  , testCase "rejects non-symbol in params" $ do
      case extractParams [Symbol "x", Integer 5] of
        Left _ -> return ()
        Right _ -> fail "Should reject non-symbol"

  , testCase "handles empty params" $ do
      extractParams [] @?= Right []
  ]

parseDefineFunctionTests :: TestTree
parseDefineFunctionTests = testGroup "parseDefineFunction"
  [ testCase "parses function definition" $ do
      case parseDefineFunction "add" [Symbol "a", Symbol "b"]
           (List [Symbol "+", Symbol "a", Symbol "b"]) of
        Right (Define name (Lambda params _ _)) -> do
          name @?= "add"
          length params @?= 2
        _ -> fail "Should parse function define"

  , testCase "rejects non-symbol parameters" $ do
      case parseDefineFunction "f" [Integer 5] (Integer 1) of
        Left _ -> return ()
        Right _ -> fail "Should reject non-symbol params"
  ]

parseDefineVariableTests :: TestTree
parseDefineVariableTests = testGroup "parseDefineVariable"
  [ testCase "parses variable definition" $ do
      case parseDefineVariable "x" (Integer 42) of
        Right (Define "x" (AstInteger 42)) -> return ()
        _ -> fail "Should parse variable define"

  , testCase "parses variable with expression" $ do
      case parseDefineVariable "y" (List [Symbol "+", Integer 1, Integer 2]) of
        Right (Define "y" (Call "+" _)) -> return ()
        _ -> fail "Should parse variable with expression"
  ]

parseLambdaTests :: TestTree
parseLambdaTests = testGroup "parseLambda"
  [ testCase "parses lambda with one param" $ do
      case parseLambda [Symbol "x"] (Integer 1) of
        Right (Lambda ["x"] (AstInteger 1) []) -> return ()
        _ -> fail "Should parse lambda"

  , testCase "parses lambda with multiple params" $ do
      case parseLambda [Symbol "x", Symbol "y"]
           (List [Symbol "+", Symbol "x", Symbol "y"]) of
        Right (Lambda params _ []) -> length params @?= 2
        _ -> fail "Should parse lambda with multiple params"

  , testCase "rejects non-symbol parameters" $ do
      case parseLambda [Integer 5] (Integer 1) of
        Left _ -> return ()
        Right _ -> fail "Should reject non-symbol params"
  ]

parseIfTests :: TestTree
parseIfTests = testGroup "parseIf"
  [ testCase "parses if expression" $ do
      case parseIf (Symbol "#t") (Integer 1) (Integer 0) of
        Right (If _ (AstInteger 1) (AstInteger 0)) -> return ()
        _ -> fail "Should parse if"

  , testCase "parses if with nested expressions" $ do
      case parseIf (List [Symbol "<", Integer 5, Integer 10])
           (List [Symbol "+", Integer 1, Integer 2])
           (Integer 0) of
        Right (If (Call "<" _) (Call "+" _) (AstInteger 0)) -> return ()
        _ -> fail "Should parse if with nested expressions"
  ]

parseFunctionCallTests :: TestTree
parseFunctionCallTests = testGroup "parseFunctionCall"
  [ testCase "parses + operator" $ do
      case parseFunctionCall (Symbol "+") [Integer 1, Integer 2] of
        Right (Call "+" [AstInteger 1, AstInteger 2]) -> return ()
        _ -> fail "Should parse + operator"

  , testCase "parses - operator" $ do
      case parseFunctionCall (Symbol "-") [Integer 5, Integer 2] of
        Right (Call "-" [AstInteger 5, AstInteger 2]) -> return ()
        _ -> fail "Should parse - operator"

  , testCase "parses * operator" $ do
      case parseFunctionCall (Symbol "*") [Integer 3, Integer 4] of
        Right (Call "*" [AstInteger 3, AstInteger 4]) -> return ()
        _ -> fail "Should parse * operator"

  , testCase "parses div operator" $ do
      case parseFunctionCall (Symbol "div") [Integer 10, Integer 2] of
        Right (Call "div" [AstInteger 10, AstInteger 2]) -> return ()
        _ -> fail "Should parse div operator"

  , testCase "parses mod operator" $ do
      case parseFunctionCall (Symbol "mod") [Integer 10, Integer 3] of
        Right (Call "mod" [AstInteger 10, AstInteger 3]) -> return ()
        _ -> fail "Should parse mod operator"

  , testCase "parses eq? operator" $ do
      case parseFunctionCall (Symbol "eq?") [Integer 5, Integer 5] of
        Right (Call "eq?" [AstInteger 5, AstInteger 5]) -> return ()
        _ -> fail "Should parse eq? operator"

  , testCase "parses < operator" $ do
      case parseFunctionCall (Symbol "<") [Integer 5, Integer 10] of
        Right (Call "<" [AstInteger 5, AstInteger 10]) -> return ()
        _ -> fail "Should parse < operator"

  , testCase "converts non-operator to list" $ do
      case parseFunctionCall (Symbol "customFunc") [Integer 1] of
        Right (AstList [AstSymbol "customFunc", AstInteger 1]) -> return ()
        _ -> fail "Should convert custom function to list"

  , testCase "handles nested function calls" $ do
      case parseFunctionCall (Symbol "+")
           [List [Symbol "*", Integer 2, Integer 3]] of
        Right (Call "+" [Call "*" _]) -> return ()
        _ -> fail "Should handle nested calls"
  ]
