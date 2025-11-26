{-# LANGUAGE OverloadedStrings #-}

module ASTSpec (astTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Lisp.AST.AST
import SExpr

astTests :: TestTree
astTests = testGroup "AST Tests"
  [ astDataConstructorTests,
    sexprToASTTests,
    compEnvTests,
    extractIntegerTests,
    handleStringTests,
    handleCallTests,
    evalASTTests,
    evalASTWithEnvTests,
    showInstanceTests,
    eqInstanceTests
  ]

astDataConstructorTests :: TestTree
astDataConstructorTests = testGroup "Ast data constructors"
  [ testCase "creates all Ast types" $ do
      let define = Define "x" (AstInteger 5)
      let call = Call "+" [AstInteger 1, AstInteger 2]
      let lambda = Lambda ["x"] (AstInteger 1) []
      let ifExpr = If (AstBoolean True) (AstInteger 1) (AstInteger 0)

      define @?= Define "x" (AstInteger 5)
      call @?= Call "+" [AstInteger 1, AstInteger 2]
      lambda @?= Lambda ["x"] (AstInteger 1) []
      ifExpr @?= If (AstBoolean True) (AstInteger 1) (AstInteger 0)
  ]

sexprToASTTests :: TestTree
sexprToASTTests = testGroup "sexprToAST"
  [ testCase "converts integers" $ do
      sexprToAST (Integer 42) @?= Just (AstInteger 42)
      sexprToAST (Integer 0) @?= Just (AstInteger 0)
      sexprToAST (Integer (-17)) @?= Just (AstInteger (-17))

  , testCase "converts symbols" $ do
      sexprToAST (Symbol "x") @?= Just (AstSymbol "x")
      sexprToAST (Symbol "hello") @?= Just (AstSymbol "hello")
      sexprToAST (Symbol "+") @?= Just (AstSymbol "+")

  , testCase "converts define expressions" $ do
      let sexpr = List [Symbol "define", Symbol "x", Integer 5]
      sexprToAST sexpr @?= Just (Define "x" (AstInteger 5))

  , testCase "rejects malformed define expressions" $ do
      sexprToAST (List [Symbol "define"]) @?= Nothing
      sexprToAST (List [Symbol "define", Symbol "x"]) @?= Nothing
      sexprToAST (List [Symbol "define", Symbol "x", Integer 1, Integer 2]) @?= Nothing

  , testCase "converts lambda expressions" $ do
      let sexpr = List [Symbol "lambda", List [Symbol "x"], Integer 1]
      sexprToAST sexpr @?= Just (Lambda ["x"] (AstInteger 1) [])

      let sexpr2 = List [Symbol "lambda", List [Symbol "x", Symbol "y"],
                        List [Symbol "+", Symbol "x", Symbol "y"]]
      sexprToAST sexpr2 @?= Just (Lambda ["x", "y"]
                    (Call "+" [AstSymbol "x", AstSymbol "y"]) [])

  , testCase "rejects malformed lambda expressions" $ do
      sexprToAST (List [Symbol "lambda"]) @?= Nothing
      sexprToAST (List [Symbol "lambda", Symbol "x"]) @?= Nothing
      sexprToAST (List [Symbol "lambda", List [Symbol "x"]]) @?= Nothing

  , testCase "converts if expressions" $ do
      let sexpr = List [Symbol "if", Symbol "#t", Integer 1, Integer 0]
      sexprToAST sexpr @?= Just (If (AstSymbol "#t") (AstInteger 1) (AstInteger 0))

  , testCase "converts function calls" $ do
      let sexpr = List [Symbol "+", Integer 1, Integer 2]
      sexprToAST sexpr @?= Just (Call "+" [AstInteger 1, AstInteger 2])

      let sexpr2 = List [Symbol "eq?", Symbol "x", Integer 5]
      sexprToAST sexpr2 @?= Just (Call "eq?" [AstSymbol "x", AstInteger 5])

  , testCase "converts non-function lists to AstList" $ do
      let sexpr = List [Symbol "unknown", Integer 1]
      sexprToAST sexpr @?= Just (AstList [AstSymbol "unknown", AstInteger 1])

  , testCase "converts empty lists" $ do
      sexprToAST (List []) @?= Just (AstList [])

  , testCase "converts regular lists" $ do
      let sexpr = List [Integer 1, Integer 2, Integer 3]
      sexprToAST sexpr @?= Just (AstList [AstInteger 1, AstInteger 2, AstInteger 3])

  , testCase "converts named function definitions (easy to read)" $ do
      let sexpr = List [Symbol "define", List [Symbol "square", Symbol "x"],
                        List [Symbol "*", Symbol "x", Symbol "x"]]
      sexprToAST sexpr @?= Just (Define "square" (Lambda ["x"]
                    (Call "*" [AstSymbol "x", AstSymbol "x"]) []))

  , testCase "converts named function with multiple parameters" $ do
      let sexpr = List [Symbol "define", List [Symbol "add", Symbol "x", Symbol "y"],
                        List [Symbol "+", Symbol "x", Symbol "y"]]
      sexprToAST sexpr @?= Just (Define "add" (Lambda ["x", "y"]
                    (Call "+" [AstSymbol "x", AstSymbol "y"]) []))
  ]

compEnvTests :: TestTree
compEnvTests = testGroup "compEnv"
  [ testCase "finds variables in environment" $ do
      let env = [("x", AstInteger 5), ("y", AstInteger 10)]
      compEnv env "x" @?= Just (AstInteger 5)
      compEnv env "y" @?= Just (AstInteger 10)

  , testCase "returns Nothing for non-existent variables" $ do
      let env = [("x", AstInteger 5)]
      compEnv env "z" @?= Nothing

  , testCase "returns Nothing for empty environment" $ do
      compEnv [] "x" @?= Nothing
  ]

extractIntegerTests :: TestTree
extractIntegerTests = testGroup "extractInteger"
  [ testCase "extracts integers from evaluated expressions" $ do
      let env = []
      extractInteger env (AstInteger 42) @?= Just 42
      extractInteger env (AstInteger 0) @?= Just 0

  , testCase "extracts integers from variables in environment" $ do
      let env = [("x", AstInteger 5)]
      extractInteger env (AstSymbol "x") @?= Just 5

  , testCase "returns Nothing for non-integer values" $ do
      let env = []
      extractInteger env (AstBoolean True) @?= Nothing
      extractInteger env (AstSymbol "undefined") @?= Nothing
  ]

handleStringTests :: TestTree
handleStringTests = testGroup "handleString"
  [ testCase "handles boolean literals" $ do
      let env = []
      handleString env "#t" @?= Just (AstBoolean True)
      handleString env "#f" @?= Just (AstBoolean False)

  , testCase "looks up variables in environment" $ do
      let env = [("x", AstInteger 5)]
      handleString env "x" @?= Just (AstInteger 5)

  , testCase "returns symbol for undefined variables" $ do
      let env = []
      handleString env "undefined" @?= Just (AstSymbol "undefined")
  ]

handleCallTests :: TestTree
handleCallTests = testGroup "handleCall"
  [ testCase "performs addition" $ do
      let env = []
      handleCall env "+" [AstInteger 3, AstInteger 2] @?= Just (AstInteger 5)
      handleCall env "+" [AstInteger 0, AstInteger 0] @?= Just (AstInteger 0)
      handleCall env "+" [AstInteger (-1), AstInteger 1] @?= Just (AstInteger 0)

  , testCase "performs subtraction" $ do
      let env = []
      handleCall env "-" [AstInteger 5, AstInteger 3] @?= Just (AstInteger 2)
      handleCall env "-" [AstInteger 0, AstInteger 5] @?= Just (AstInteger (-5))

  , testCase "performs multiplication" $ do
      let env = []
      handleCall env "*" [AstInteger 3, AstInteger 4] @?= Just (AstInteger 12)
      handleCall env "*" [AstInteger 0, AstInteger 5] @?= Just (AstInteger 0)

  , testCase "performs division" $ do
      let env = []
      handleCall env "div" [AstInteger 10, AstInteger 3] @?= Just (AstInteger 3)
      handleCall env "div" [AstInteger 6, AstInteger 2] @?= Just (AstInteger 3)

  , testCase "handles division by zero" $ do
      let env = []
      handleCall env "div" [AstInteger 10, AstInteger 0] @?= Nothing

  , testCase "performs modulo" $ do
      let env = []
      handleCall env "mod" [AstInteger 10, AstInteger 3] @?= Just (AstInteger 1)
      handleCall env "mod" [AstInteger 6, AstInteger 2] @?= Just (AstInteger 0)

  , testCase "handles modulo by zero" $ do
      let env = []
      handleCall env "mod" [AstInteger 10, AstInteger 0] @?= Nothing

  , testCase "performs equality comparison" $ do
      let env = []
      handleCall env "eq?" [AstInteger 5, AstInteger 5] @?= Just (AstBoolean True)
      handleCall env "eq?" [AstInteger 3, AstInteger 5] @?= Just (AstBoolean False)

  , testCase "performs less than comparison" $ do
      let env = []
      handleCall env "<" [AstInteger 3, AstInteger 5] @?= Just (AstBoolean True)
      handleCall env "<" [AstInteger 5, AstInteger 3] @?= Just (AstBoolean False)

  , testCase "works with variables from environment" $ do
      let env = [("x", AstInteger 10), ("y", AstInteger 3)]
      handleCall env "+" [AstSymbol "x", AstSymbol "y"] @?= Just (AstInteger 13)
      handleCall env "<" [AstSymbol "y", AstSymbol "x"] @?= Just (AstBoolean True)

  , testCase "returns Nothing for invalid operations" $ do
      let env = []
      handleCall env "unknown" [AstInteger 1, AstInteger 2] @?= Nothing
      handleCall env "+" [AstInteger 1] @?= Nothing
      handleCall env "+" [] @?= Nothing
      handleCall env "+" [AstBoolean True, AstInteger 2] @?= Nothing
  ]

evalASTTests :: TestTree
evalASTTests = testGroup "evalAST"
  [ testCase "evaluates integers to themselves" $ do
      let env = []
      evalAST env (AstInteger 42) @?= Just (AstInteger 42)

  , testCase "evaluates booleans to themselves" $ do
      let env = []
      evalAST env (AstBoolean True) @?= Just (AstBoolean True)
      evalAST env (AstBoolean False) @?= Just (AstBoolean False)

  , testCase "evaluates symbols" $ do
      let env = [("x", AstInteger 5)]
      evalAST env (AstSymbol "x") @?= Just (AstInteger 5)
      evalAST env (AstSymbol "#t") @?= Just (AstBoolean True)

  , testCase "evaluates function calls" $ do
      let env = []
      evalAST env (Call "+" [AstInteger 1, AstInteger 2]) @?= Just (AstInteger 3)

  , testCase "evaluates if expressions" $ do
      let env = []
      evalAST env (If (AstBoolean True) (AstInteger 1) (AstInteger 0)) @?= Just (AstInteger 1)
      evalAST env (If (AstBoolean False) (AstInteger 1) (AstInteger 0)) @?= Just (AstInteger 0)

  , testCase "evaluates lambda expressions" $ do
      let env = []
      let lambda = Lambda ["x"] (AstInteger 1) []
      evalAST env lambda @?= Just lambda

  , testCase "evaluates define expressions" $ do
      let env = []
      evalAST env (Define "x" (AstInteger 5)) @?= Just (AstSymbol "")

  , testCase "evaluates empty lists" $ do
      let env = []
      evalAST env (AstList []) @?= Just (AstList [])

  , testCase "evaluates single-element lists" $ do
      let env = []
      evalAST env (AstList [AstInteger 42]) @?= Just (AstInteger 42)

  , testCase "evaluates function calls in lists" $ do
      let env = []
      evalAST env (AstList [AstSymbol "+", AstInteger 1, AstInteger 2]) @?= Just (AstInteger 3)

  , testCase "evaluates lambda application with single argument" $ do
      let env = []
      let lambda = Lambda ["x"] (Call "*" [AstSymbol "x", AstInteger 2]) []
      evalAST env (AstList [lambda, AstInteger 5]) @?= Just (AstInteger 10)

  , testCase "evaluates lambda application with multiple arguments" $ do
      let env = []
      let lambda = Lambda ["x", "y"] (Call "+" [AstSymbol "x", AstSymbol "y"]) []
      evalAST env (AstList [lambda, AstInteger 3, AstInteger 7]) @?= Just (AstInteger 10)

  , testCase "evaluates lambda with closure" $ do
      let env = [("z", AstInteger 10)]
      let lambda = Lambda ["x"] (Call "+" [AstSymbol "x", AstSymbol "z"]) env
      evalAST [] (AstList [lambda, AstInteger 5]) @?= Just (AstInteger 15)

  , testCase "rejects lambda application with wrong number of arguments" $ do
      let env = []
      let lambda = Lambda ["x", "y"] (Call "+" [AstSymbol "x", AstSymbol "y"]) []
      evalAST env (AstList [lambda, AstInteger 5]) @?= Nothing
  ]

evalASTWithEnvTests :: TestTree
evalASTWithEnvTests = testGroup "evalASTWithEnv"
  [ testCase "returns Nothing for empty list" $ do
      evalASTWithEnv [] [] @?= Nothing

  , testCase "evaluates single expression" $ do
      evalASTWithEnv [] [AstInteger 42] @?= Just (AstInteger 42)

  , testCase "handles define statements" $ do
      let exprs = [Define "x" (AstInteger 5), AstSymbol "x"]
      evalASTWithEnv [] exprs @?= Just (AstInteger 5)

  , testCase "handles multiple defines" $ do
      let exprs = [Define "x" (AstInteger 5),
                  Define "y" (AstInteger 10),
                  Call "+" [AstSymbol "x", AstSymbol "y"]]
      evalASTWithEnv [] exprs @?= Just (AstInteger 15)

  , testCase "handles named function definition and call" $ do
      let exprs = [Define "double" (Lambda ["x"] (Call "*" [AstSymbol "x", AstInteger 2]) []),
                  AstList [AstSymbol "double", AstInteger 5]]
      evalASTWithEnv [] exprs @?= Just (AstInteger 10)

  , testCase "handles named function with multiple parameters" $ do
      let exprs = [Define "add" (Lambda ["x", "y"] (Call "+" [AstSymbol "x", AstSymbol "y"]) []),
                  AstList [AstSymbol "add", AstInteger 3, AstInteger 7]]
      evalASTWithEnv [] exprs @?= Just (AstInteger 10)

  , testCase "handles closure capturing environment" $ do
      let exprs = [Define "x" (AstInteger 5),
                  Define "addX" (Lambda ["y"] (Call "+" [AstSymbol "x", AstSymbol "y"]) []),
                  AstList [AstSymbol "addX", AstInteger 3]]
      evalASTWithEnv [] exprs @?= Just (AstInteger 8)

  , testCase "handles nested lambda calls" $ do
      let exprs = [Define "makeAdder" (Lambda ["x"] (Lambda ["y"] (Call "+" [AstSymbol "x", AstSymbol "y"]) []) []),
                  Define "add5" (AstList [AstSymbol "makeAdder", AstInteger 5]),
                  AstList [AstSymbol "add5", AstInteger 3]]
      evalASTWithEnv [] exprs @?= Just (AstInteger 8)
  ]

showInstanceTests :: TestTree
showInstanceTests = testGroup "Show instance (deriving)"
  [ testCase "shows AstInteger values using Show" $ do
      show (AstInteger 42) @?= "AstInteger 42"
      show (AstInteger (-17)) @?= "AstInteger (-17)"

  , testCase "shows AstBoolean values using Show" $ do
      show (AstBoolean True) @?= "AstBoolean True"
      show (AstBoolean False) @?= "AstBoolean False"

  , testCase "shows AstSymbol values using Show" $ do
      show (AstSymbol "test") @?= "AstSymbol \"test\""
      show (AstSymbol "") @?= "AstSymbol \"\""
  ]

eqInstanceTests :: TestTree
eqInstanceTests = testGroup "Eq instance (deriving)"
  [ testCase "compares AstInteger values for equality" $ do
      (AstInteger 42 == AstInteger 42) @?= True
      (AstInteger 42 == AstInteger 43) @?= False

  , testCase "compares AstBoolean values for equality" $ do
      (AstBoolean True == AstBoolean True) @?= True
      (AstBoolean True == AstBoolean False) @?= False

  , testCase "compares AstSymbol values for equality" $ do
      (AstSymbol "test" == AstSymbol "test") @?= True
      (AstSymbol "test" == AstSymbol "other") @?= False
  ]
