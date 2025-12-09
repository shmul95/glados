# S-Expression to AST Conversion

## What is this?

This module handles the conversion from S-expressions (the parsed representation of Lisp code) to AST (Abstract Syntax Tree). It validates the syntax and transforms S-expressions into a more structured form that can be evaluated.

## Main Conversion Function

### sexprToAST

```haskell
sexprToAST :: SExpr -> Either String Ast
```

The primary function that converts an S-expression into an AST node. Returns `Left errorMessage` if the S-expression is invalid or malformed, `Right ast` on success.

**Error messages** are defined in the `Lisp.AST.ASTError` module and provide detailed information about what went wrong.

## Simple Conversions

### Integers

Numbers are directly converted:
```haskell
sexprToAST (Integer n) = Right (AstInteger n)
```

**Example:**
```haskell
sexprToAST (Integer 42)  -- Right (AstInteger 42)
```

### Symbols

Symbols are converted to AST symbols, except for reserved keywords (`define`, `lambda`, `if`):
```haskell
sexprToAST (Symbol s)
    | isReservedSymbol s = Left (reservedSymbolError s)
    | otherwise = Right (AstSymbol s)
```

**Examples:**
```haskell
sexprToAST (Symbol "x")       -- Right (AstSymbol "x")
sexprToAST (Symbol "+")       -- Right (AstSymbol "+")
sexprToAST (Symbol "define")  -- Left "'define' is a reserved symbol and cannot be redefined."
```

### Lists

Lists are parsed using the internal `parseList` function:
```haskell
sexprToAST (List sexprs) = parseList sexprs
```

An empty list returns an error:
```haskell
parseList [] = Left emptySExprError
```

## Special Forms

Special forms are Lisp constructs with special syntax and evaluation rules.

### Define - Function Definition

Syntax: `(define (funcName param1 param2 ...) body)`

This is syntactic sugar for defining a function. It creates a lambda and binds it to the function name.

```haskell
parseDefineFunction :: String -> [SExpr] -> SExpr -> Either String Ast
parseDefineFunction funcName params body = do
    astBody <- sexprToAST body
    paramNames <- extractParams params
    let lambda = Lambda paramNames astBody []
    return $ Define funcName lambda
```

**Example:**
```scheme
(define (square x) (* x x))
```
Converts to:
```haskell
Define "square" (Lambda ["x"] (Call "*" [AstSymbol "x", AstSymbol "x"]) [])
```

### Define - Variable Definition

Syntax: `(define varName value)`

Simple variable binding.

```haskell
parseDefineVariable :: String -> SExpr -> Either String Ast
parseDefineVariable varName valueExpr = do
    astValue <- sexprToAST valueExpr
    return $ Define varName astValue
```

**Example:**
```scheme
(define x 42)
```
Converts to:
```haskell
Define "x" (AstInteger 42)
```

**Validation:** Any other form of `define` (wrong number of arguments, etc.) returns `Left invalidDefineError`:
```haskell
parseList (Symbol "define" : _) = Left invalidDefineError
-- "Invalid 'define' syntax. Expected (define name value) or (define (name args) body)."
```

### Lambda - Anonymous Functions

Syntax: `(lambda (param1 param2 ...) body)`

Creates an anonymous function with parameters and a body.

```haskell
parseLambda :: [SExpr] -> SExpr -> Either String Ast
parseLambda args body = do
    astBody <- sexprToAST body
    astArgs <- extractParams args
    return $ Lambda astArgs astBody []
```

**Example:**
```scheme
(lambda (x y) (+ x y))
```
Converts to:
```haskell
Lambda ["x", "y"] (Call "+" [AstSymbol "x", AstSymbol "y"]) []
```

Note: The empty list `[]` is the initial closure environment, which will be filled during evaluation.

**Validation:** Malformed lambda expressions return `Left invalidLambdaError`:
```haskell
parseList (Symbol "lambda" : _) = Left invalidLambdaError
-- "Invalid 'lambda' syntax. Expected (lambda (args) body)."
```

### If - Conditional Expression

Syntax: `(if condition thenExpr elseExpr)`

All three parts are required.

```haskell
parseIf :: SExpr -> SExpr -> SExpr -> Either String Ast
parseIf condExpr thenExpr elseExpr = do
    astCond <- sexprToAST condExpr
    astThen <- sexprToAST thenExpr
    astElse <- sexprToAST elseExpr
    return $ If astCond astThen astElse
```

**Example:**
```scheme
(if (< x 0) 0 x)
```
Converts to:
```haskell
If (Call "<" [AstSymbol "x", AstInteger 0])
   (AstInteger 0)
   (AstSymbol "x")
```

**Validation:** Malformed if expressions return `Left invalidIfError`:
```haskell
parseList (Symbol "if" : _) = Left invalidIfError
-- "Invalid 'if' syntax. Expected (if condition then-expression else-expression)."
```

## Function Calls

Syntax: `(func arg1 arg2 ...)`

Function calls are detected by checking if the first element is a built-in operator.

```haskell
parseFunctionCall :: SExpr -> [SExpr] -> Either String Ast
parseFunctionCall funcExpr argExprs = do
    astFunc <- sexprToAST funcExpr
    astArgs <- mapM sexprToAST argExprs
    case astFunc of
        AstSymbol op | op `elem` ["+", "-", "*", "div", "mod", "eq?", "<"] ->
            return $ Call op astArgs
        _ -> return $ AstList (astFunc : astArgs)
```

**Built-in operators:** `+`, `-`, `*`, `div`, `mod`, `eq?`, `<`

**Examples:**

Built-in operator call:
```scheme
(+ 1 2)
```
Converts to:
```haskell
Call "+" [AstInteger 1, AstInteger 2]
```

User-defined function call:
```scheme
(square 5)
```
Converts to:
```haskell
AstList [AstSymbol "square", AstInteger 5]
```

### Generic Lists

Any list that doesn't match a special form pattern is converted to an `AstList`:

```haskell
sexprToAST (List exprs) =
    fmap AstList (mapM sexprToAST exprs)
```

## Helper Functions

### extractSymbol

```haskell
extractSymbol :: SExpr -> Either String String
```

Safely extracts a string from a `Symbol` S-expression. Returns `Left "Expected symbol"` if not a symbol.

### extractParams

```haskell
extractParams :: [SExpr] -> Either String [String]
```

Extracts parameter names from a list of S-expressions. All elements must be symbols.

**Example:**
```haskell
extractParams [Symbol "x", Symbol "y"]  -- Right ["x", "y"]
extractParams [Symbol "x", Integer 1]   -- Left "Expected symbol"
```

### isReservedSymbol

```haskell
isReservedSymbol :: String -> Bool
```

Checks if a string is a reserved keyword that cannot be used as a variable name.

Reserved symbols: `define`, `lambda`, `if`

## Validation and Error Handling

The conversion process validates syntax at every step:

- ✅ **Returns `Right Ast`** for valid S-expressions
- ❌ **Returns `Left errorMessage`** for:
  - Empty lists: `Left emptySExprError`
  - Reserved keywords used as variable names: `Left (reservedSymbolError name)`
  - Malformed `define`: `Left invalidDefineError`
  - Malformed `lambda`: `Left invalidLambdaError`
  - Malformed `if`: `Left invalidIfError`
  - Invalid parameter lists (non-symbols): `Left "Expected symbol"`
  - Any structural errors propagated from nested conversions

**Error messages** are defined in `Lisp.AST.ASTError` module and provide clear, actionable information about what went wrong.

## Conversion Pipeline

```
Source Code
    ↓
[Parser Module]
    ↓
SExpr
    ↓
[sexprToAST] ← This module
    ↓
AST
    ↓
[Evaluation Module]
    ↓
Result
```

## Complete Example

Input Lisp code:
```scheme
(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))
```

Parsed to S-expression:
```haskell
List [
  Symbol "define",
  List [Symbol "factorial", Symbol "n"],
  List [
    Symbol "if",
    List [Symbol "eq?", Symbol "n", Integer 0],
    Integer 1,
    List [
      Symbol "*",
      Symbol "n",
      List [Symbol "factorial", List [Symbol "-", Symbol "n", Integer 1]]
    ]
  ]
]
```

Converted to AST:
```haskell
Define "factorial" (Lambda ["n"]
  (If (Call "eq?" [AstSymbol "n", AstInteger 0])
      (AstInteger 1)
      (Call "*" [
        AstSymbol "n",
        AstList [
          AstSymbol "factorial",
          Call "-" [AstSymbol "n", AstInteger 1]
        ]
      ]))
  [])
```

This AST is then ready for evaluation by the AST module.
