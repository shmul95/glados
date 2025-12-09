# AST Error Messages Module

## What is this?

The ASTError module centralizes all error messages for the Lisp interpreter. It provides consistent, descriptive error messages for both AST conversion failures and evaluation errors.

## Purpose

By centralizing error messages:
- **Consistency**: All errors follow the same format and style
- **Maintainability**: Easy to update error messages in one place
- **Clarity**: Users get helpful, actionable error information
- **Localization-ready**: Could be adapted for multiple languages

## Error Categories

### AST Conversion Errors

These errors occur when converting S-expressions to AST (in the `SExprToAST` module).

#### reservedSymbolError

```haskell
reservedSymbolError :: String -> String
```

Triggered when trying to use a reserved keyword as a variable name.

**Usage:**
```haskell
reservedSymbolError "define"
-- "'define' is a reserved symbol and cannot be redefined."
```

**Example:**
```scheme
define  ; Error: 'define' is a reserved symbol and cannot be redefined.
```

#### invalidDefineError

```haskell
invalidDefineError :: String
```

Triggered by malformed `define` expressions.

**Message:**
```
Invalid 'define' syntax. Expected (define name value) or (define (name args) body).
```

**Examples:**
```scheme
(define)           ; Missing arguments
(define x)         ; Missing value
(define x y z)     ; Too many arguments
```

#### invalidLambdaError

```haskell
invalidLambdaError :: String
```

Triggered by malformed `lambda` expressions.

**Message:**
```
Invalid 'lambda' syntax. Expected (lambda (args) body).
```

**Examples:**
```scheme
(lambda)           ; Missing arguments and body
(lambda (x))       ; Missing body
(lambda x x)       ; Args must be in a list
```

#### invalidIfError

```haskell
invalidIfError :: String
```

Triggered by malformed `if` expressions.

**Message:**
```
Invalid 'if' syntax. Expected (if condition then-expression else-expression).
```

**Examples:**
```scheme
(if)               ; Missing all parts
(if #t 1)          ; Missing else branch
(if #t 1 2 3)      ; Too many branches
```

#### emptySExprError

```haskell
emptySExprError :: String
```

Triggered when trying to convert an empty S-expression list to AST.

**Message:**
```
Empty expression cannot be converted to AST.
```

**Example:**
```scheme
()  ; Error: Empty expression cannot be converted to AST.
```

### Evaluation Errors

These errors occur during AST evaluation (in the `AST` module).

#### undefinedVariableError

```haskell
undefinedVariableError :: String -> String
```

Triggered when referencing a variable that doesn't exist in the environment.

**Usage:**
```haskell
undefinedVariableError "x"
-- "Undefined variable: x"
```

**Example:**
```scheme
x  ; Error: Undefined variable: x
(+ y 5)  ; Error: Undefined variable: y
```

#### divisionByZeroError

```haskell
divisionByZeroError :: String
```

Triggered when dividing or taking modulo by zero.

**Message:**
```
Division by zero.
```

**Examples:**
```scheme
(div 5 0)  ; Error: Division by zero.
(mod 10 0) ; Error: Division by zero.
```

#### argumentMustBeIntegerError

```haskell
argumentMustBeIntegerError :: String
```

Triggered when an arithmetic operation receives a non-integer argument.

**Message:**
```
Argument must be an integer.
```

**Example:**
```scheme
(+ #t 5)  ; Error: Argument must be an integer.
```

#### unknownOperatorError

```haskell
unknownOperatorError :: String -> String
```

Triggered when using an unrecognized operator.

**Usage:**
```haskell
unknownOperatorError "%"
-- "Unknown operator: %"
```

**Example:**
```scheme
(% 10 3)  ; Error: Unknown operator: %
```

#### operatorRequiresTwoArgumentsError

```haskell
operatorRequiresTwoArgumentsError :: String
```

Triggered when a binary operator receives the wrong number of arguments.

**Message:**
```
Operator requires exactly 2 arguments.
```

**Examples:**
```scheme
(+ 1)      ; Error: Operator requires exactly 2 arguments.
(+ 1 2 3)  ; Error: Operator requires exactly 2 arguments.
```

#### conditionMustBeBooleanError

```haskell
conditionMustBeBooleanError :: String
```

Triggered when an `if` condition evaluates to a non-boolean value.

**Message:**
```
Condition must be a boolean value.
```

**Example:**
```scheme
(if 5 1 2)   ; Error: Condition must be a boolean value.
(if x 1 2)   ; Error if x is not #t or #f
```

#### functionExpectsArgumentsError

```haskell
functionExpectsArgumentsError :: String -> Int -> Int -> String
```

Triggered when a function is called with the wrong number of arguments.

**Usage:**
```haskell
functionExpectsArgumentsError "square" 1 2
-- "Function 'square' expects 1 argument(s) but got 2"
```

**Example:**
```scheme
(define (square x) (* x x))
(square 5 10)  ; Error: Function 'square' expects 1 argument(s) but got 2
```

#### invalidListExpressionError

```haskell
invalidListExpressionError :: String
```

Triggered when evaluating an invalid list expression.

**Message:**
```
Invalid list expression.
```

#### emptyListError

```haskell
emptyListError :: String
```

Triggered when trying to evaluate an empty list.

**Message:**
```
Attempted to evaluate an empty list.
```

## Implementation Pattern

All error functions follow a simple pattern:

```haskell
-- Static error messages
simpleError :: String
simpleError = "Error description."

-- Parameterized error messages
contextualError :: String -> String
contextualError param = "Error description involving " ++ param ++ "."

-- Multi-parameter error messages
detailedError :: String -> Int -> Int -> String
detailedError name expected actual =
    "Function '" ++ name ++ "' expects " ++ show expected ++
    " argument(s) but got " ++ show actual
```

## Usage in Code

### In SExprToAST module:

```haskell
sexprToAST (Symbol s)
    | isReservedSymbol s = Left (reservedSymbolError s)
    | otherwise = Right (AstSymbol s)

parseList [] = Left emptySExprError
parseList (Symbol "define" : _) = Left invalidDefineError
```

### In AST module:

```haskell
handleString env s =
    case compEnv env s of
        Just value -> evalAST env value
        _          -> (env, Left (undefinedVariableError s))

guardNonZero b result = if b /= 0
    then Right result
    else Left divisionByZeroError
```

### In Executor module:

```haskell
executeSExpr env sexpr =
    case sexprToAST sexpr of
        Left err -> (env, Left ("AST conversion error: " ++ err))
        Right ast -> ...
```

## Benefits

1. **User-friendly**: Error messages are clear and explain what went wrong
2. **Consistent**: All errors follow the same pattern
3. **Debuggable**: Errors include relevant context (variable names, operators, etc.)
4. **Maintainable**: Changing error messages requires editing only this module
5. **Type-safe**: The type system ensures errors are used correctly

## Future Enhancements

Potential improvements:
- Add error codes for programmatic error handling
- Include source location information (line/column numbers)
- Support multiple languages for internationalization
- Add suggestions for common mistakes
- Group related errors into data types
