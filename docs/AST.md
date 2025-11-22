# AST and Evaluation

## What is this?

This module takes the parsed S-expressions from our Lisp code and converts them into an Abstract Syntax Tree (AST). Then it evaluates these expressions line by line.

## The AST Structure

Our AST can represent different kinds of Lisp expressions:

```haskell
data Ast = Define Ast Ast          -- (define x 5)
    | Call String [Ast]            -- (+ 1 2)
    | Lambda [Ast] Ast             -- (lambda (x) (* x x))
    | If Ast Ast Ast               -- (if #t x 0)
    | AstInteger Int               -- 42
    | AstSymbol String             -- x, foo, +
    | AstBoolean Bool              -- #t, #f
    | AstList [Ast]                -- lists and programs
```

Each constructor handles a specific type of expression:
- `Define` - variable definitions
- `Call` - function calls
- `Lambda` - anonymous functions
- `If` - conditional expressions
- The rest are basic data types

## Converting S-expressions to AST

The main function `sexprToAST :: SExpr -> Maybe Ast` return the result of Lisp code

### Simple cases

Numbers and symbols are simple:
```haskell
sexprToAST (Integer n) = Just (AstInteger n)
sexprToAST (Symbol s)  = Just (AstSymbol s)
```

### Special forms

**Variable definitions** need exactly 3 parts: `(define variable value)`
```haskell
sexprToAST (List [Symbol "define", varName, valueExpr]) = do
    astValue <- sexprToAST valueExpr
    astVarName <- sexprToAST varName
    Just (Define astVarName astValue)
```

**Lambda functions** expect: `(lambda (param1 param2 ...) body)`
```haskell
sexprToAST (List [Symbol "lambda", List args, body]) = do
    astValue <- sexprToAST body
    astArgs <- mapM sexprToAST args
    Just (Lambda astArgs astValue)
```

**Function calls** are detected by looking at the first symbol. If it's one of our built-in operators (`+`, `-`, `*`, etc.), we create a `Call`. Otherwise, it becomes a generic list.

## How evaluation works

### The Environment

We keep track of variables using a simple list of pairs:
```haskell
type Environment = [(Ast, Ast)]
```

So after running `(define x 5)(define y 10)`, our environment looks like:
```haskell
[(AstSymbol "x", AstInteger 5), (AstSymbol "y", AstInteger 10)]
```

### The main evaluator

`evalAST :: Environment -> Ast -> Maybe Ast` takes an AST expression and tries to evaluate it with the environment.

**Numbers and booleans** are just numbers or booleans, there are nothing more simple.

**Symbols** are where it gets interesting. We check if it's `#t` or `#f` first (our boolean literals), then look it up in the environment. If we can't find it, we just return the symbol string.

**Function calls** like `(+ 1 2)` get handled by `handleCall`, which do arithmetic and comparisons with arguments.

**If statements** evaluate the condition first, then pick the right branch based on whether it's true or false.

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