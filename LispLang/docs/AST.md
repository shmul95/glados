# AST and Evaluation

## What is this?

This module takes the parsed S-expressions from our Lisp code and converts them into an Abstract Syntax Tree (AST). Then it evaluates these expressions line by line.

## The AST Structure

Our AST can represent different kinds of Lisp expressions:

```haskell
data Ast = Define String Ast          -- (define x 5)
    | Call String [Ast]               -- (+ 1 2)
    | Lambda [String] Ast Environment -- (lambda (x) (* x x))
    | If Ast Ast Ast                  -- (if #t x 0)
    | AstInteger Int                  -- 42
    | AstSymbol String                -- x, foo, +
    | AstBoolean Bool                 -- #t, #f
    | AstList [Ast]                   -- lists and programs
```

Each constructor handles a specific type of expression:
- `Define` - variable definitions
- `Call` - function calls
- `Lambda` - anonymous functions
- `If` - conditional expressions
- The rest are basic data types

## How evaluation works

### The Environment

We keep track of variables using a simple list of pairs:
```haskell
type Environment = [(String, Ast)]
```

So after running `(define x 5)(define y 10)`, our environment looks like:
```haskell
[("x", AstInteger 5), ("y", AstInteger 10)]
```

### The main evaluator

`evalAST :: Environment -> Ast -> (Environment, Either String Ast)` takes an AST expression and evaluates it with the environment. It returns both the (potentially updated) environment and either an error message or the result.

**Numbers and booleans** evaluate to themselves: `(env, Right (AstInteger n))` or `(env, Right (AstBoolean b))`.

**Symbols** are handled by `handleString`, which:
- Returns `(env, Right (AstBoolean True))` for `#t`
- Returns `(env, Right (AstBoolean False))` for `#f`
- Looks up other symbols in the environment and evaluates them
- Returns `(env, Left (undefinedVariableError s))` if the variable is not found

**Function calls** like `(+ 1 2)` are handled by `handleOpt`, which:
- Validates that exactly 2 arguments are provided
- Extracts integer values from the arguments
- Applies the operation and returns `(env, Right result)` or `(env, Left error)`

**If statements** are handled by `handleCondition`, which:
- Evaluates the condition
- Requires the condition to be a boolean, otherwise returns `Left conditionMustBeBooleanError`
- Evaluates the appropriate branch based on the boolean value

**Lambda evaluation** captures the current environment as a closure: `(env, Right (Lambda params body env))`. When a lambda is called (via `AstList` pattern):
1. Validates argument count matches parameter count
2. Evaluates all arguments in the current environment
3. Creates a new environment with the function bound to itself (for recursion), parameter bindings, and the closure environment
4. Evaluates the body in this new environment

This ensures proper lexical scoping and enables recursive functions.

**Definitions** are handled by `handleDefine`, which:
- Evaluates the value expression
- On success: adds the binding to the environment and returns `((name, val) : env, Right (AstSymbol ""))`
- On failure: propagates the error without modifying the environment

### Arithmetic and comparisons

We support these built-in arithmetic functions which give an integer:
- `+`, `-`, `*` - basic arithmetic
- `div`, `mod` - integer division and remainder
And these built-in comparisons functions which give a boolean:
- `eq?` - equality test
- `<` - less than comparison

### Running multiple expressions

The key function is `evalASTWithEnv :: Environment -> [Ast] -> (Environment, Either String Ast)`. This lets us run a sequence of expressions, threading the environment through each evaluation.

**Behavior:**
- Empty list: Returns `(env, Left emptyListError)`
- Single expression: Evaluates it and returns the result
- Multiple expressions: Evaluates each in sequence, short-circuiting on the first error

Here's what happens with `(define x 5)(+ x 2)`:

1. Start with empty environment: `[]`
2. Process `(define x 5)`: `handleDefine` evaluates `5`, returns `(("x", AstInteger 5) : [], Right (AstSymbol ""))`
3. Process `(+ x 2)`: look up `x` in `[("x", AstInteger 5)]` (gets `5`), compute `5 + 2 = 7`
4. Return `([("x", AstInteger 5)], Right (AstInteger 7))`

### Helper functions

The module exports several helper functions:
- `compEnv :: Environment -> String -> Maybe Ast` - looks up a variable in the environment
- `extractInteger :: Environment -> Ast -> Maybe Int` - evaluates an AST and extracts an integer value (discards error messages, returns Nothing on failure)
- `handleString :: Environment -> String -> (Environment, Either String Ast)` - handles string symbols and boolean literals (#t, #f), returns detailed errors for undefined variables
- `handleCall :: Environment -> String -> [String] -> Ast -> Environment -> [Ast] -> (Environment, Either String Ast)` - handles user-defined function calls with proper closure support. Validates argument count and returns detailed errors.

**Internal helpers (not exported):**
- `extractIntegerOrError :: Environment -> Ast -> Either String Int` - extracts an integer or returns a detailed error message
- `guardNonZero :: Int -> Ast -> Either String Ast` - checks for division by zero
- `applyOp :: String -> Int -> Int -> Either String Ast` - applies arithmetic/comparison operators with error handling
- `validateArgCount :: Int -> [Ast] -> Either String [Ast]` - validates argument count
- `handleOpt :: Environment -> String -> [Ast] -> Either String Ast` - handles built-in arithmetic and comparison operators with validation
- `evaluateArg :: Environment -> Ast -> Ast` - evaluates an argument, returning AstSymbol "" on error (used in function calls)
- `handleCondition :: Environment -> Ast -> Ast -> Ast -> (Environment, Either String Ast)` - evaluates if expressions, requires boolean condition
- `handleDefine :: Environment -> String -> Ast -> (Environment, Either String Ast)` - handles variable and function definitions, propagates errors

**Pipeline:**
```
Source Code → Parser → SExpr → SExprToAST → AST → Evaluation
```
