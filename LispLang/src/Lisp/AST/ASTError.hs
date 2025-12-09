module Lisp.AST.ASTError (
    reservedSymbolError,
    invalidDefineError,
    invalidLambdaError,
    invalidIfError,
    emptySExprError,
    undefinedVariableError,
    divisionByZeroError,
    argumentMustBeIntegerError,
    unknownOperatorError,
    operatorRequiresTwoArgumentsError,
    conditionMustBeBooleanError,
    functionExpectsArgumentsError,
    invalidListExpressionError,
    emptyListError
) where

-- Error message definitions for SExpr to AST conversion

reservedSymbolError :: String -> String
reservedSymbolError name =
    "'" ++ name ++ "' is a reserved symbol and cannot be redefined."

invalidDefineError :: String
invalidDefineError = "Invalid 'define' syntax. " ++
    "Expected (define name value) or (define (name args) body)."

invalidLambdaError :: String
invalidLambdaError = "Invalid 'lambda' syntax. Expected (lambda (args) body)."

invalidIfError :: String
invalidIfError = "Invalid 'if' syntax. " ++
    "Expected (if condition then-expression else-expression)."

emptySExprError :: String
emptySExprError = "Empty expression cannot be converted to AST."

-- Error message definitions for AST evaluation
undefinedVariableError :: String -> String
undefinedVariableError name = "Undefined variable: " ++ name

divisionByZeroError :: String
divisionByZeroError = "Division by zero."

emptyListError :: String
emptyListError = "Attempted to evaluate an empty list."

argumentMustBeIntegerError :: String
argumentMustBeIntegerError = "Argument must be an integer."

unknownOperatorError :: String -> String
unknownOperatorError op = "Unknown operator: " ++ op

operatorRequiresTwoArgumentsError :: String
operatorRequiresTwoArgumentsError = "Operator requires exactly 2 arguments."

conditionMustBeBooleanError :: String
conditionMustBeBooleanError = "Condition must be a boolean value."

functionExpectsArgumentsError :: String -> Int -> Int -> String
functionExpectsArgumentsError funcName expected actual =
    "Function '" ++ funcName ++ "' expects " ++ show expected ++
    " argument(s) but got " ++ show actual

invalidListExpressionError :: String
invalidListExpressionError = "Invalid list expression."