# Executor Module

## What is this?

The Executor module is the main orchestrator that brings together all the components of the Lisp interpreter. It takes raw Lisp source code as a string and executes it through the complete pipeline: parsing, AST conversion, and evaluation.

## Main Execution Function

### executeLispWithEnv

```haskell
executeLispWithEnv :: Environment -> String -> (Environment, Either String Ast)
```

The primary entry point for executing Lisp code. Takes an environment and source code string, returns the updated environment and either an error message or the resulting AST.

**Parameters:**
- `env :: Environment` - The current variable environment
- `input :: String` - The Lisp source code to execute

**Returns:**
- `(Environment, Either String Ast)` - A tuple containing:
  - The updated environment after execution
  - Either an error message (`Left String`) or the result (`Right Ast`)

**Execution flow:**
1. Parse the input string into S-expressions
2. If parsing fails, return a parse error
3. If parsing succeeds, convert S-expressions to AST and evaluate

**Example:**
```haskell
let (newEnv, result) = executeLispWithEnv [] "(+ 1 2)"
-- newEnv = []
-- result = Right (AstInteger 3)

let (env2, result2) = executeLispWithEnv newEnv "(define x 5)"
-- env2 = [("x", AstInteger 5)]
-- result2 = Right (AstSymbol "")
```

## Internal Execution Functions

### executeSExpr

```haskell
executeSExpr :: Environment -> SExpr -> (Environment, Either String Ast)
```

Converts an S-expression to AST and evaluates it.

**Handles two cases:**

1. **List of expressions** - Processes multiple expressions sequentially:
```haskell
executeSExpr env (List sexprs) =
    case mapM sexprToAST sexprs of
        Nothing -> (env, Left "AST conversion error")
        Just asts -> executeAsts (evalASTWithEnv env asts)
```

2. **Single expression** - Processes one expression:
```haskell
executeSExpr env sexpr =
    case sexprToAST sexpr of
        Nothing -> (env, Left "AST conversion error")
        Just ast -> executeAsts (evalAST env ast)
```

**Error handling:** Returns `"AST conversion error"` if the S-expression cannot be converted to valid AST.

### executeAsts

```haskell
executeAsts :: (Environment, Maybe Ast) -> (Environment, Either String Ast)
```

Converts the result from evaluation (which uses `Maybe`) into the `Either` type for better error reporting.

- `(env, Just ast)` → `(env, Right ast)` - Success
- `(env, Nothing)` → `(env, Left "Evaluation error")` - Failure

## Output Conversion

### astToString

```haskell
astToString :: Ast -> String
```

Converts an AST result into a human-readable string representation for display.

**Conversion rules:**

| AST Type | Output |
|----------|--------|
| `AstInteger n` | The number as string (e.g., `"42"`) |
| `AstBoolean True` | `"#t"` |
| `AstBoolean False` | `"#f"` |
| `AstSymbol s` | The symbol string (e.g., `"foo"`) |
| `Lambda _ _ _` | `"#<procedure>"` |
| Anything else | `""` (empty string) |

**Examples:**
```haskell
astToString (AstInteger 42)        -- "42"
astToString (AstBoolean True)      -- "#t"
astToString (AstBoolean False)     -- "#f"
astToString (AstSymbol "x")        -- "x"
astToString (Lambda ["x"] _ [])    -- "#<procedure>"
astToString (Define "x" _)         -- ""
```

## Helper Functions

### maybeToEither

```haskell
maybeToEither :: String -> Maybe a -> Either String a
```

Utility function to convert `Maybe` values to `Either` with a custom error message.

- `Just x` → `Right x`
- `Nothing` → `Left errMsg`

## Complete Execution Pipeline

```
Input String
    ↓
[parseLispDocument] (Parser module)
    ↓
SExpr
    ↓
[sexprToAST] (SExprToAST module)
    ↓
AST
    ↓
[evalAST / evalASTWithEnv] (AST module)
    ↓
(Environment, Maybe Ast)
    ↓
[executeAsts] (converts Maybe to Either)
    ↓
(Environment, Either String Ast)
    ↓
[astToString] (for display)
    ↓
String output
```

## Error Handling

The executor handles three types of errors:

1. **Parse errors**: When the input is not valid Lisp syntax
   ```haskell
   executeLispWithEnv [] "(+ 1"
   -- ([], Left "Parse error: ...")
   ```

2. **AST conversion errors**: When S-expressions don't form valid AST structures
   ```haskell
   executeLispWithEnv [] "(define)"
   -- ([], Left "AST conversion error")
   ```

3. **Evaluation errors**: When evaluation fails (e.g., undefined variables, division by zero)
   ```haskell
   executeLispWithEnv [] "(div 1 0)"
   -- ([], Left "Evaluation error")
   ```

## Environment Threading

The executor properly threads the environment through the execution:

```haskell
-- Start with empty environment
let (env1, _) = executeLispWithEnv [] "(define x 5)"
-- env1 = [("x", AstInteger 5)]

-- Use the updated environment for the next execution
let (env2, result) = executeLispWithEnv env1 "(+ x 10)"
-- env2 = [("x", AstInteger 5)]  (unchanged)
-- result = Right (AstInteger 15)
```

This allows for stateful execution where definitions persist across multiple evaluations.

## Usage Example

```haskell
-- Define a function and use it
let code = "(define (square x) (* x x))"
let (env1, r1) = executeLispWithEnv [] code
-- env1 = [("square", Lambda ["x"] ...)]
-- r1 = Right (AstSymbol "")

let (env2, r2) = executeLispWithEnv env1 "(square 5)"
-- env2 = [("square", Lambda ["x"] ...)]
-- r2 = Right (AstInteger 25)

-- Convert result to string for display
case r2 of
    Right ast -> astToString ast  -- "25"
    Left err -> err
```
