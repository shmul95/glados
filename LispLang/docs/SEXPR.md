# S-Expressions (SExpr)

## What is this?

This module defines the S-expression data structure, which is the fundamental syntax representation for Lisp code. S-expressions are the parsed form of Lisp source code before being converted to an AST.

## The SExpr Structure

An S-expression can be one of three things:

```haskell
data SExpr = Integer Int    -- A number: 42
    | Symbol String         -- A symbol: foo, +, define
    | List [SExpr]         -- A list of S-expressions: (+ 1 2)
    deriving (Show, Eq)
```

This simple structure can represent any Lisp program:
- **Integer**: Numeric literals
- **Symbol**: Variable names, function names, keywords
- **List**: Function calls, special forms, or data lists

### Examples

```scheme
42              → Integer 42
foo             → Symbol "foo"
(+ 1 2)         → List [Symbol "+", Integer 1, Integer 2]
(define x 5)    → List [Symbol "define", Symbol "x", Integer 5]
```

## Accessor Functions

The module provides safe accessor functions that return `Maybe` values:

### getSymbol

```haskell
getSymbol :: SExpr -> Maybe String
```

Extracts a string from a `Symbol`. Returns `Just` the string if it's a symbol, `Nothing` otherwise.

```haskell
getSymbol (Symbol "foo")  -- Just "foo"
getSymbol (Integer 42)    -- Nothing
```

### getInteger

```haskell
getInteger :: SExpr -> Maybe Int
```

Extracts an integer from an `Integer`. Returns `Just` the number if it's an integer, `Nothing` otherwise.

```haskell
getInteger (Integer 42)   -- Just 42
getInteger (Symbol "foo") -- Nothing
```

### getList

```haskell
getList :: SExpr -> Maybe [SExpr]
```

Extracts the list contents from a `List`. Returns `Just` the list if it's a list, `Nothing` otherwise.

```haskell
getList (List [Symbol "+", Integer 1, Integer 2])  -- Just [Symbol "+", Integer 1, Integer 2]
getList (Integer 42)                               -- Nothing
```

## Pretty Printing Functions

The module provides functions to generate human-readable descriptions of S-expressions:

### printTree

```haskell
printTree :: SExpr -> Maybe String
```

Converts an S-expression into a descriptive string. This is useful for debugging and displaying parse results.

**Examples:**
```haskell
printTree (Symbol "foo")
-- Just "a Symbol foo"

printTree (Integer 42)
-- Just "a Number 42"

printTree (List [Symbol "+", Integer 1, Integer 2])
-- Just "a List with a Symbol + followed by a Number 1, a Number 2"

printTree (List [])
-- Just "a List with nothing in it"
```

### describeList

```haskell
describeList :: [SExpr] -> Maybe String
```

Helper function that describes the contents of a list. Used internally by `printTree`.

- Empty list → `"nothing in it"`
- Non-empty list → Describes first element "followed by" the rest

### describeListRest

```haskell
describeListRest :: [SExpr] -> Maybe String
```

Helper function that describes the remaining elements in a list, separating them with commas.

- Empty list → `""`
- Single element → Description of that element
- Multiple elements → Comma-separated descriptions

## Usage Example

```haskell
-- Parsing "(+ 1 2)" gives us:
let sexpr = List [Symbol "+", Integer 1, Integer 2]

-- We can extract parts:
getSymbol (Symbol "+")     -- Just "+"
getInteger (Integer 1)     -- Just 1

-- Or get a readable description:
printTree sexpr
-- Just "a List with a Symbol + followed by a Number 1, a Number 2"
```

## Integration with Parser

This module is used by the Parser module, which takes raw Lisp source code and converts it into S-expressions. The S-expressions are then converted to AST by the `SExprToAST` module.

**Pipeline:**
```
Source Code → Parser → SExpr → SExprToAST → AST → Evaluation
```
