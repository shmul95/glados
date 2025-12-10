# Main Module

## What is this?

The Main module is the entry point of the Lisp interpreter. It provides both a REPL (Read-Eval-Print Loop) for interactive use and a script execution mode for running Lisp files.

## Program Entry Point

### main

```haskell
main :: IO ()
```

The main function handles command-line arguments and determines the execution mode:

**Three modes of operation:**

1. **No arguments + terminal (REPL mode)**
   ```bash
   ./glados
   > (+ 1 2)
   3
   > (define x 5)

   > (+ x 10)
   15
   > exit
   ```

2. **No arguments + piped input (script mode)**
   ```bash
   echo "(+ 1 2)" | ./glados
   3
   ```

3. **One argument (file execution)**
   ```bash
   ./glados script.lisp
   ```

**Error handling:**
```bash
./glados file1.lisp file2.lisp
Usage: glados [script.lisp]
```

## REPL Mode

### lispLoop

```haskell
lispLoop :: Environment -> IO ()
```

The interactive Read-Eval-Print Loop that maintains state across multiple inputs.

**Features:**
- Displays a prompt: `> `
- Reads user input
- Evaluates the input
- Prints the result
- Maintains the environment between evaluations

**Flow:**
1. Print prompt `> ` and flush output
2. Read a line of input
3. Trim whitespace
4. Handle the input
5. Repeat with updated environment

### handleInput

```haskell
handleInput :: Environment -> String -> IO ()
```

Processes a single line of user input in the REPL.

**Behavior:**

| Input | Action |
|-------|--------|
| Empty string `""` | Ignore and continue REPL |
| `"exit"` | Exit the REPL |
| Lisp code | Execute and display result, continue with updated environment |

**Example session:**
```
> (define x 5)

> (define y 10)

> (+ x y)
15
> exit
```

**Environment persistence:** The environment is threaded through the loop, so definitions persist:
```haskell
handleInput env input = do
    let (newEnv, result) = executeLispWithEnv env input
    either putStrLn (putStrLn . astToString) result
    lispLoop newEnv  -- Uses newEnv for next iteration
```

## Script Execution Mode

### processInput

```haskell
processInput :: String -> IO ()
```

Executes Lisp code from a string (file or stdin) and prints the result.

**Characteristics:**
- Uses an empty initial environment
- Doesn't maintain state (one-shot execution)
- Prints result to stdout
- Prints errors to stderr

**Example:**
```haskell
-- Content of script.lisp:
-- (define x 5)
-- (+ x 10)

processInput content
-- Output: 15
```

**Error handling:**
```haskell
processInput "(+ 1 invalid)"
-- stderr: Parse error: ...
```

## Utility Functions

### trim

```haskell
trim :: String -> String
```

Removes leading and trailing whitespace from a string.

**Implementation:**
```haskell
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")
```

This removes whitespace characters (space, tab, newline, carriage return) from both ends.

**Examples:**
```haskell
trim "  hello  "     -- "hello"
trim "\n\t(+ 1 2)"   -- "(+ 1 2)"
trim ""              -- ""
```

## Output Handling

### Success vs Error Output

The program distinguishes between successful results and errors:

**REPL mode:**
- Success: Print to stdout
- Error: Print to stdout (for interactive feedback)

**Script mode:**
- Success: Print to stdout
- Error: Print to stderr (follows Unix conventions)

```haskell
-- REPL: both to stdout
either putStrLn (putStrLn . astToString) result

-- Script: errors to stderr
either (hPutStrLn stderr) (putStrLn . astToString) result
```

## Command-Line Interface

### Usage Pattern

```bash
glados [script.lisp]
```

### Mode Detection

The program automatically detects the execution mode:

```haskell
main = getArgs >>= \case
    [] -> hIsTerminalDevice stdin >>= \case
        True -> lispLoop []      -- Interactive REPL
        False -> getContents >>= processInput  -- Piped input
    [filename] -> readFile filename >>= processInput  -- File execution
    _ -> hPutStrLn stderr "Usage: glados [script.lisp]"  -- Error
```

**Terminal detection:** Uses `hIsTerminalDevice stdin` to differentiate between:
- Interactive terminal → Start REPL
- Piped/redirected input → Execute as script

## Complete Usage Examples

### 1. Interactive REPL

```bash
$ ./glados
> (define (factorial n)
    (if (eq? n 0)
        1
        (* n (factorial (- n 1)))))

> (factorial 5)
120
> exit
```

### 2. File Execution

```bash
$ cat factorial.lisp
(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 10)

$ ./glados factorial.lisp
3628800
```

### 3. Piped Input

```bash
$ echo "(+ (* 2 3) 4)" | ./glados
10
```

### 4. Error Handling

```bash
$ ./glados nonexistent.lisp another.lisp
Usage: glados [script.lisp]

$ echo "(+ 1" | ./glados
Parse error: ...
```

## Environment Management

### REPL Mode (Stateful)

Environment persists across evaluations:
```haskell
lispLoop env = do
    -- ... read input ...
    let (newEnv, result) = executeLispWithEnv env input
    -- ... print result ...
    lispLoop newEnv  -- Next iteration uses updated environment
```

### Script Mode (Stateless)

Each execution starts fresh:
```haskell
processInput input = do
    let (_, result) = executeLispWithEnv [] input  -- Empty environment
    -- ... print result ...
```

## IO Operations

The module uses several IO operations:

- `getArgs` - Get command-line arguments
- `hIsTerminalDevice` - Check if running in a terminal
- `getLine` - Read interactive input
- `getContents` - Read all of stdin
- `readFile` - Read file contents
- `putStr`/`putStrLn` - Write to stdout
- `hPutStrLn stderr` - Write to stderr
- `hFlush stdout` - Ensure prompt is displayed immediately

## Language Extensions

```haskell
{-# LANGUAGE LambdaCase #-}
```

Uses the `LambdaCase` extension for cleaner pattern matching syntax:
```haskell
getArgs >>= \case
    [] -> ...
    [filename] -> ...
    _ -> ...
```
