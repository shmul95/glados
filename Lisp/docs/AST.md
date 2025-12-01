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

`evalAST :: Environment -> Ast -> (Environment, Maybe Ast)` takes an AST expression and tries to evaluate it with the environment. It returns both the (potentially updated) environment and the result.

**Numbers and booleans** evaluate to themselves: `(env, Just (AstInteger n))` or `(env, Just (AstBoolean b))`.

**Symbols** are handled by `handleString`, which checks if it's `#t` or `#f` first (boolean literals), then looks it up in the environment. If found, it evaluates the value; otherwise returns `(env, Nothing)`.

**Function calls** like `(+ 1 2)` get handled by `handleOpt`, which performs arithmetic and comparisons with arguments. The result is wrapped as `(env, handleOpt env op args)`.

**If statements** are handled by `handleCondition`, which evaluates the condition first, then picks the right branch based on whether it's true or false, returning the updated environment and result.

**Lambda evaluation** captures the current environment as a closure: `(env, Just (Lambda params body env))`. When a lambda is called (via `AstList` pattern):
1. The function's arguments are evaluated in the current environment
2. Parameters are bound to the evaluated arguments
3. A new environment is created: the function binds to itself (for recursion), then parameter bindings, then the closure environment
4. The body is evaluated in this new environment

This ensures proper lexical scoping and enables recursive functions.

**Definitions** are handled by `handleDefine`, which evaluates the value and adds it to the environment, returning `((name, val) : env, Just (AstSymbol ""))`.

### Arithmetic and comparisons

We support these built-in arithmetic functions which give an integer:
- `+`, `-`, `*` - basic arithmetic
- `div`, `mod` - integer division and remainder
And these built-in comparisons functions which give a boolean:
- `eq?` - equality test
- `<` - less than comparison

### Running multiple expressions

The key function is `evalASTWithEnv :: Environment -> [Ast] -> (Environment, Maybe Ast)`. This lets us run a sequence of expressions, threading the environment through each evaluation.

For each expression, we evaluate it with the current environment and use the returned environment for the next expression. The final result is the evaluation of the last expression along with the final environment state.

Here's what happens with `(define x 5)(+ x 2)`:

1. Start with empty environment: `[]`
2. Process `(define x 5)`: `handleDefine` evaluates `5`, returns `(("x", AstInteger 5) : [], Just (AstSymbol ""))`
3. Process `(+ x 2)`: look up `x` in `[("x", AstInteger 5)]` (gets `5`), compute `5 + 2 = 7`
4. Return `([("x", AstInteger 5)], Just (AstInteger 7))`

### Helper functions

The module exports several helper functions:
- `compEnv :: Environment -> String -> Maybe Ast` - looks up a variable in the environment
- `extractInteger :: Environment -> Ast -> Maybe Int` - evaluates an AST and extracts an integer value (note: this uses evalAST internally and discards the environment)
- `handleString :: Environment -> String -> (Environment, Maybe Ast)` - handles string symbols and boolean literals (#t, #f)
- `handleCall :: Environment -> String -> [String] -> Ast -> Environment -> [Ast] -> (Environment, Maybe Ast)` - handles user-defined function calls with proper closure support. Takes the function name, parameter names, body, closure environment, and arguments.

**Internal helpers (not exported):**
- `handleOpt :: Environment -> String -> [Ast] -> Maybe Ast` - handles built-in arithmetic and comparison operators
- `handleCondition :: Environment -> Ast -> Ast -> Ast -> (Environment, Maybe Ast)` - evaluates if expressions
- `handleDefine :: Environment -> String -> Ast -> (Environment, Maybe Ast)` - handles variable and function definitions, updating the environment

**Pipeline:**
```
Source Code → Parser → SExpr → SExprToAST → AST → Evaluation
```
