{-# LANGUAGE OverloadedStrings #-}

module AST.ASTErrorSpec (astErrorTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Lisp.AST.ASTError

--
-- This test module is useless because it's just testing error messages
-- (that is obviously useless).
-- We keep it just for the coverage :)
--

astErrorTests :: TestTree
astErrorTests = testGroup "AST Error Tests"
  [ reservedSymbolErrorTests
  , invalidDefineErrorTests
  , invalidLambdaErrorTests
  , invalidIfErrorTests
  , emptySExprErrorTests
  , undefinedVariableErrorTests
  , divisionByZeroErrorTests
  , argumentMustBeIntegerErrorTests
  , unknownOperatorErrorTests
  , operatorRequiresTwoArgumentsErrorTests
  , conditionMustBeBooleanErrorTests
  , functionExpectsArgumentsErrorTests
  , invalidListExpressionErrorTests
  , emptyListErrorTests
  ]

reservedSymbolErrorTests :: TestTree
reservedSymbolErrorTests = testGroup "reservedSymbolError"
  [ testCase "generates error for define" $ do
      reservedSymbolError "define" @?= "'define' is a reserved symbol and cannot be redefined."

  , testCase "generates error for lambda" $ do
      reservedSymbolError "lambda" @?= "'lambda' is a reserved symbol and cannot be redefined."

  , testCase "generates error for if" $ do
      reservedSymbolError "if" @?= "'if' is a reserved symbol and cannot be redefined."
  ]

invalidDefineErrorTests :: TestTree
invalidDefineErrorTests = testGroup "invalidDefineError"
  [ testCase "returns define syntax error" $ do
      invalidDefineError @?= "Invalid 'define' syntax. Expected (define name value) or (define (name args) body)."
  ]

invalidLambdaErrorTests :: TestTree
invalidLambdaErrorTests = testGroup "invalidLambdaError"
  [ testCase "returns lambda syntax error" $ do
      invalidLambdaError @?= "Invalid 'lambda' syntax. Expected (lambda (args) body)."
  ]

invalidIfErrorTests :: TestTree
invalidIfErrorTests = testGroup "invalidIfError"
  [ testCase "returns if syntax error" $ do
      invalidIfError @?= "Invalid 'if' syntax. Expected (if condition then-expression else-expression)."
  ]

emptySExprErrorTests :: TestTree
emptySExprErrorTests = testGroup "emptySExprError"
  [ testCase "returns empty expression error" $ do
      emptySExprError @?= "Empty expression cannot be converted to AST."
  ]

undefinedVariableErrorTests :: TestTree
undefinedVariableErrorTests = testGroup "undefinedVariableError"
  [ testCase "generates error for undefined variable" $ do
      undefinedVariableError "x" @?= "Undefined variable: x"

  , testCase "generates error for another variable" $ do
      undefinedVariableError "myVar" @?= "Undefined variable: myVar"
  ]

divisionByZeroErrorTests :: TestTree
divisionByZeroErrorTests = testGroup "divisionByZeroError"
  [ testCase "returns division by zero error" $ do
      divisionByZeroError @?= "Division by zero."
  ]

argumentMustBeIntegerErrorTests :: TestTree
argumentMustBeIntegerErrorTests = testGroup "argumentMustBeIntegerError"
  [ testCase "returns argument type error" $ do
      argumentMustBeIntegerError @?= "Argument must be an integer."
  ]

unknownOperatorErrorTests :: TestTree
unknownOperatorErrorTests = testGroup "unknownOperatorError"
  [ testCase "generates error for unknown operator" $ do
      unknownOperatorError "foo" @?= "Unknown operator: foo"

  , testCase "generates error for another operator" $ do
      unknownOperatorError "bar" @?= "Unknown operator: bar"
  ]

operatorRequiresTwoArgumentsErrorTests :: TestTree
operatorRequiresTwoArgumentsErrorTests = testGroup "operatorRequiresTwoArgumentsError"
  [ testCase "returns operator argument count error" $ do
      operatorRequiresTwoArgumentsError @?= "Operator requires exactly 2 arguments."
  ]

conditionMustBeBooleanErrorTests :: TestTree
conditionMustBeBooleanErrorTests = testGroup "conditionMustBeBooleanError"
  [ testCase "returns condition type error" $ do
      conditionMustBeBooleanError @?= "Condition must be a boolean value."
  ]

functionExpectsArgumentsErrorTests :: TestTree
functionExpectsArgumentsErrorTests = testGroup "functionExpectsArgumentsError"
  [ testCase "generates error with correct counts" $ do
      functionExpectsArgumentsError "add" 2 1 @?=
        "Function 'add' expects 2 argument(s) but got 1"

  , testCase "generates error for multiple arguments" $ do
      functionExpectsArgumentsError "myFunc" 3 5 @?=
        "Function 'myFunc' expects 3 argument(s) but got 5"
  ]

invalidListExpressionErrorTests :: TestTree
invalidListExpressionErrorTests = testGroup "invalidListExpressionError"
  [ testCase "returns invalid list error" $ do
      invalidListExpressionError @?= "Invalid list expression."
  ]

emptyListErrorTests :: TestTree
emptyListErrorTests = testGroup "emptyListError"
  [ testCase "returns empty list error" $ do
      emptyListError @?= "Attempted to evaluate an empty list."
  ]
