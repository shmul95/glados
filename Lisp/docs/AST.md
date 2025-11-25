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

## Converting S-expressions to AST

The main function `sexprToAST :: SExpr -> Maybe Ast` returns the result of Lisp code

### Simple cases

Numbers and symbols are simple:
```haskell
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s)  = Just (AstSymbol s)
```

### Special forms

**Variable definitions** can take two forms:

1. Simple variable definition: `(define variable value)`
```haskell
sexprToAST (List [Symbol "define", Symbol s, valueExpr]) = do
    astValue <- sexprToAST valueExpr
    Just (Define s astValue)
```

2. Function definition (syntactic sugar): `(define (funcName param1 param2) body)`
```haskell
sexprToAST (List [Symbol "define", List (Symbol funcName : params), body]) = do
    astBody <- sexprToAST body
    paramNames <- mapM extractParam params
    Just (Define funcName (Lambda paramNames astBody []))
```

**Lambda functions** expect: `(lambda (param1 param2 ...) body)`
```haskell
sexprToAST (List [Symbol "lambda", List args, body]) = do
    astValue <- sexprToAST body
    let extractParam se = case se of
            Symbol s -> Just s
            _ -> Nothing
    astArgs <- mapM extractParam args
    Just (Lambda astArgs astValue [])
```

Note: Lambda captures the current environment as a closure for proper lexical scoping.

**Function calls** are detected by looking at the first symbol. If it's one of our built-in operators (`+`, `-`, `*`, etc.), we create a `Call`. Otherwise, it becomes a generic list.

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

`evalAST :: Environment -> Ast -> Maybe Ast` takes an AST expression and tries to evaluate it with the environment.

**Numbers and booleans** are just numbers or booleans, there are nothing more simple.

**Symbols** are where it gets interesting. We check if it's `#t` or `#f` first (our boolean literals), then look it up in the environment. If we can't find it, we just return the symbol string.

**Function calls** like `(+ 1 2)` get handled by `handleCall`, which do arithmetic and comparisons with arguments.

**If statements** evaluate the condition first, then pick the right branch based on whether it's true or false.

**Lambda evaluation** is handled specially. When a lambda is called:
1. The function's arguments are evaluated
2. Parameters are bound to the evaluated arguments
3. A new environment is created by combining the bindings with the lambda's closure environment
4. The body is evaluated in this new environment

This ensures proper lexical scoping and closure behavior.

### Arithmetic and comparisons

We support these built-in arithmetic functions which give an integer:
- `+`, `-`, `*` - basic arithmetic
- `div`, `mod` - integer division and remainder
And these built-in comparisons functions which give a boolean:
- `eq?` - equality test
- `<` - less than comparison

### Running multiple expressions

The key function is `evalASTWithEnv :: Environment -> [Ast] -> Maybe Ast`. This lets us run a sequence of expressions, updating the environment as we go.

When we hit a `define`, we evaluate the value and add it to the environment for the next expressions. For everything else, we just evaluate it and move on.

Here's what happens with `(define x 5)(+ x 2)`:

1. Start with empty environment: `[]`
2. Process `(define x 5)`: evaluate `5`, add `x → 5` to environment
3. Process `(+ x 2)`: look up `x` (gets `5`), compute `5 + 2 = 7`
4. Return `7`

### Helper functions

The module exports several helper functions:
- `compEnv :: Environment -> String -> Maybe Ast` - looks up a variable in the environment
- `extractInteger :: Environment -> Ast -> Maybe Int` - evaluates an AST and extracts an integer value
- `handleString :: Environment -> String -> Maybe Ast` - handles string symbols and boolean literals (#t, #f)
- `handleCall :: Environment -> String -> [Ast] -> Maybe Ast` - handles built-in function calls
- `handleCondition :: Environment -> Ast -> Ast -> Ast -> Maybe Ast` - evaluates if expressions (internal helper)