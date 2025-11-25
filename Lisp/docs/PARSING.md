# Parsing and S-Expressions

## What's this about?

The parsing system is how we turn raw Lisp text into something our interpreter can actually work with. It happens in two main steps:

1. **Text → S-expressions** (this module)
2. **S-expressions → AST** (covered in the AST docs)

S-expressions are the direct representation of code that is composed of different types of data

## S-Expressions (SExpr)

### The data structure

```haskell
data SExpr = Integer Int
    | Symbol String
    | List [SExpr]
    deriving (Show, Eq)
```

S-expressions are pretty simple. Each type of data represent one or several things:
- `Integer Int` - Numbers like `42`, `-17` or `0`
- `Symbol String` - Names and operators like `x`, `define`, `+` or `#t`
- `List [SExpr]` - Parenthesized expressions like `(+ 1 2)` or `(define x 5)`

### Helper functions

We have some utility functions to extract values safely:

```haskell
getSymbol :: SExpr -> Maybe String    -- Get a symbol's name
getInteger :: SExpr -> Maybe Int      -- Get an integer's value
getList :: SExpr -> Maybe [SExpr]     -- Get a list's contents
```

These return `Nothing` if you try to extract the wrong type (like asking for a symbol from a number).

### Pretty printing

There's also `printTree` which gives you a human-readable description of what an S-expression contains:

```haskell
printTree (Integer 42)                           -- "a Number 42"
printTree (Symbol "foo")                         -- "a Symbol 'foo'"
printTree (List [Symbol "+", Integer 1, Integer 2])
-- "a List with a Symbol '+' followed by a Number 1, a Number 2"
```

### Examples of S-expressions

Simple stuff:
```haskell
Integer 5                    -- just the number 5
Symbol "x"                   -- a variable name
Symbol "#t"                  -- boolean true
```

More complex expressions:
```haskell
-- (+ 3 2)
List [Symbol "+", Integer 3, Integer 2]

-- (define x 5)
List [Symbol "define", Symbol "x", Integer 5]

-- Nested: (if (> x 0) x 0)
List [Symbol "if",
      List [Symbol ">", Symbol "x", Integer 0],
      Symbol "x",
      Integer 0]
```

## The Parser

We use Parsec, which is a pretty nice parsing library for Haskell. It lets us build up complex parsers from simple pieces, and gives good error messages when things go wrong.

### Main entry point

`parseLispDocument` is where it all starts:

```haskell
parseLispDocument = do
    spaces                    -- Skip any leading whitespace
    exprs <- many parseLispValue  -- Parse zero or more expressions
    eof                       -- Make sure we've consumed all input
    return (List exprs)       -- Wrap everything in a list
```

This means if you give it something like:
```lisp
(define x 5)
(+ x 2)
```

You get back:
```haskell
List [List [Symbol "define", Symbol "x", Integer 5],
      List [Symbol "+", Symbol "x", Integer 2]]
```

### The individual parsers

#### `parseLispValue`

This is the main dispatcher. It tries to parse different types of values in order:

```haskell
parseLispValue = do
    spaces
    choice [parseLispArray, parseLispNumber, parseLispString]
```

It tries lists first (since they start with `(`), then numbers, then falls back to symbols.

#### `parseLispNumber`

Handles integers, including negative ones:

```haskell
parseLispNumber = do
    spaces
    sign <- optional (char '-')       -- Maybe a minus sign
    digits <- some digitChar          -- At least one digit
    let num = read digits :: Int
    return $ Integer $ case sign of
        Just _ -> -num
        Nothing -> num
```

So `"42"` becomes `Integer 42`, and `"-17"` becomes `Integer (-17)`.

#### `parseLispString`

Parses symbols and identifiers:

```haskell
parseLispString = do
    spaces
    word <- some (oneOf validChars)
    return (Symbol word)
  where
    validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['#', '<', '-', '+', '*', '/', '_', '=', '>', '!', '?']
```

This is pretty permissive. We allow letters, common operators, and some special characters like `#` (for `#t` and `#f`) and `?` (for functions like `eq?`).

#### `parseLispArray`

Handles parenthesized lists:

```haskell
parseLispArray = do
    spaces
    char '('                          -- Must start with (
    exprs <- many parseLispValue      -- Zero or more expressions inside
    spaces
    char ')'                          -- Must end with )
    return (List exprs)
```

This handles nested structures automatically since `parseLispValue` can call back into `parseLispArray`.

## How it all fits together

The overall flow is:
```
"(+ 1 2)" → Parser → SExpr → AST → Evaluation → Result
```

So if you start with the text `"(define x 5)(+ x 2)"`:

1. **Parser** gives you the S-expression structure
2. **AST converter** turns special forms like `define` into proper AST nodes
3. **Evaluator** actually runs the code

## Error handling

Parsec gives us pretty decent error messages. If you write something like `"(+ 1 2"` (missing the closing paren), you'll get a ParseError that tells you what was expected and where.

We catch these in the main parsing function and just return `Nothing` if anything goes wrong.

## Common pitfalls

- **Spaces matter**: `(+1 2)` won't parse because there's no space after the `+`
- **Parentheses must match**: `(+ 1 2` will fail
- **No quotes**: `'x` isn't supported, just `x`
- **Case sensitive**: `Define` and `define` are different symbols

The parser is pretty forgiving about whitespace though - `(  +   1   2  )` works just fine.