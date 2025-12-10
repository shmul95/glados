# Parser Module

## What is this?

The Parser module converts raw Lisp source code (text strings) into S-expressions using the Megaparsec parsing library. This is the first step in the interpretation pipeline.

## Parser Type

```haskell
type Parser = Parsec Void String
```

The parser uses Megaparsec with:
- `Void` as the custom error type (no custom errors)
- `String` as the input stream type

## Main Parsing Function

### parseLispDocument

```haskell
parseLispDocument :: Parser SExpr
```

The entry point for parsing a complete Lisp document. It parses one or more expressions and wraps them in a list.

**Behavior:**
```haskell
parseLispDocument = do
    space                      -- Skip leading whitespace
    exprs <- some parseLispValue  -- Parse at least one expression
    space                      -- Skip trailing whitespace
    eof                        -- Ensure all input is consumed
    return (List exprs)        -- Wrap in a List
```

**Examples:**

Single expression:
```lisp
(+ 1 2)
```
Parses to:
```haskell
List [List [Symbol "+", Integer 1, Integer 2]]
```

Multiple expressions:
```lisp
(define x 5)
(+ x 2)
```
Parses to:
```haskell
List [List [Symbol "define", Symbol "x", Integer 5],
      List [Symbol "+", Symbol "x", Integer 2]]
```

**Note:** Uses `some` which requires at least one expression. Empty input will fail.

## Value Parsers

### parseLispValue

```haskell
parseLispValue :: Parser SExpr
```

Parses a single Lisp value (number, symbol, or list). Skips whitespace before and after.

```haskell
parseLispValue = do
    space
    choice [parseLispArray, parseLispNumber, parseLispString]
    <* space
```

**Parsing order:**
1. Try `parseLispArray` (lists starting with `(`)
2. Try `parseLispNumber` (integers with optional `-`)
3. Fall back to `parseLispString` (symbols)

### parseLispNumber

```haskell
parseLispNumber :: Parser SExpr
```

Parses integer literals, including negative numbers.

**Implementation:**
```haskell
parseLispNumber = try $ do
    sign <- optional (char '-')
    digits <- some digitChar
    notFollowedBy (alphaNumChar <|> oneOf "!?_-+*/=<>#")
    let num = read digits :: Int
    return $ Integer $ case sign of
        Just _ -> -num
        Nothing -> num
```

**Features:**
- Optional minus sign for negative numbers
- At least one digit required
- Uses `notFollowedBy` to ensure numbers don't run into identifiers
- Wrapped in `try` to allow backtracking if it fails

**Examples:**
```haskell
"42"    → Integer 42
"-17"   → Integer (-17)
"0"     → Integer 0
"-0"    → Integer 0
```

**Not allowed:**
- `"42x"` (number followed by letter)
- `"42+"` (number followed by operator character)

### parseLispString

```haskell
parseLispString :: Parser SExpr
```

Parses symbols and identifiers.

**Implementation:**
```haskell
parseLispString = do
    first <- letterChar <|> oneOf "!?_-+*/=<>#"
    rest <- many (alphaNumChar <|> oneOf "!?_-+*/=<>#")
    let word = first : rest
    return (Symbol word)
```

**Allowed characters:**
- **First character:** letter or one of `!?_-+*/=<>#`
- **Remaining characters:** alphanumeric or one of `!?_-+*/=<>#`

**Examples:**
```haskell
"foo"     → Symbol "foo"
"x"       → Symbol "x"
"+"       → Symbol "+"
"eq?"     → Symbol "eq?"
"#t"      → Symbol "#t"
"<"       → Symbol "<"
"foo-bar" → Symbol "foo-bar"
"test123" → Symbol "test123"
```

**Note:** No validation of reserved keywords happens here - that's done during AST conversion.

### parseLispArray

```haskell
parseLispArray :: Parser SExpr
```

Parses parenthesized lists.

**Implementation:**
```haskell
parseLispArray = do
    _ <- char '('
    space
    exprs <- many parseLispValue
    space
    _ <- char ')'
    return (List exprs)
```

**Features:**
- Allows nested lists (since `parseLispValue` can recursively parse arrays)
- Allows empty lists `()`
- Whitespace flexible

**Examples:**

Simple list:
```lisp
(+ 1 2)
```
→ `List [Symbol "+", Integer 1, Integer 2]`

Nested list:
```lisp
(if (< x 0) 0 x)
```
→ `List [Symbol "if", List [Symbol "<", Symbol "x", Integer 0], Integer 0, Symbol "x"]`

Empty list:
```lisp
()
```
→ `List []`

## Whitespace Handling

The parser uses `space` (from Megaparsec.Char) which skips:
- Spaces
- Tabs
- Newlines
- Carriage returns

Whitespace is handled:
- At the start of the document
- Before and after each value
- Between elements in lists
- At the end of the document

**Example:** All of these are equivalent:
```lisp
(+ 1 2)
(  +   1   2  )
(+
  1
  2)
```

## Parser Combinators Used

The module uses several Megaparsec combinators:

- `space` - Skip whitespace
- `some` - One or more occurrences
- `many` - Zero or more occurrences
- `choice` - Try alternatives in order
- `char` - Match a specific character
- `digitChar` - Match a digit [0-9]
- `letterChar` - Match a letter [a-zA-Z]
- `alphaNumChar` - Match alphanumeric character
- `oneOf` - Match one of several characters
- `optional` - Maybe match something
- `notFollowedBy` - Negative lookahead
- `try` - Allow backtracking
- `eof` - Match end of input
- `<*` - Sequence but discard right result

## Error Handling

The parser returns `ParseErrorBundle` on failure, which includes:
- Location of the error (line and column)
- What was expected
- What was found

Example error:
```haskell
parse parseLispDocument "" "(+ 1"
-- Error at 1:5: unexpected end of input, expecting ')' or a value
```

## Integration with the Pipeline

```
Input String
    ↓
[parseLispDocument] ← This module
    ↓
SExpr
    ↓
[sexprToAST]
    ↓
AST
    ↓
[evalAST]
    ↓
Result
```

The parser is used by the Executor module:
```haskell
case parse parseLispDocument "" input of
    Left err -> -- Handle parse error
    Right sexpr -> -- Continue to AST conversion
```

## Usage Example

```haskell
import Text.Megaparsec (parse, errorBundlePretty)
import Lisp.Parser.Parser (parseLispDocument)

-- Parse some code
let input = "(define x 5)(+ x 2)"
case parse parseLispDocument "" input of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right sexpr -> print sexpr
    -- Output: List [List [Symbol "define", Symbol "x", Integer 5],
    --               List [Symbol "+", Symbol "x", Integer 2]]
```

## Limitations

- No support for floating-point numbers (only integers)
- No string literals (everything is symbols or numbers)
- No quote syntax (`'x` or `(quote x)`)
- No comments
- No character literals
- No escape sequences
