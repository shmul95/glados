# Rune Semantic Analysis - Variable Validation

This document explains the three main error cases that the Rune semantic analyzer detects for variable validation.

## Error Types

### 1. UndefinedVar Error

**Description**: Occurs when trying to use a variable that hasn't been declared in the current scope.

**Code Location**: `Vars.hs` lines 89-93

**Error Message Format**: 
```
UndefinedVar: <variable_name> doesn't exist in the scope
```

**Example**:
```rune
def main() -> null
{
    a = 2;
    b = c;  // Error: 'c' is undefined
}
```

**Output**:
```
UndefinedVar: c doesn't exist in the scope
```

### 2. TypeOverwrite Error

**Description**: Occurs when trying to redeclare a variable with a different type than it already has.

**Code Location**: `Vars.hs` lines 116-118

**Error Message Format**:
```
TypeOverwrite: <variable_name> has already type <type>
```

**Example**:
```rune
def main() -> null
{
    let x: i32 = 42;
    let x: f32 = 3.14;  // Error: x already has type i32
}
```

**Output**:
```
TypeOverwrite: x has already type TypeF32
```

### 3. MultipleType Error

**Description**: Occurs when declaring a variable with an explicit type that doesn't match the inferred type from the expression.

**Code Location**: `Vars.hs` lines 131-135

**Error Message Format**:
```
MultipleType: <variable_name> is <declared_type> and <inferred_type>
```

**Example**:
```rune
def main() -> null
{
    let x: i32 = 3.14;  // Error: declared i32 but expression is f32
}
```

**Output**:
```
MultipleType: x is Just TypeI32 and TypeF32
```

## How Variable Validation Works

### Scope Management

Variables are tracked using a `VarStack` (HashMap) that maintains variable names and their types within each scope. Each function creates a new scope initialized with its parameters.

### Validation Process

1. **Variable Declaration**: When a `StmtVarDecl` is encountered:
   - The expression type is inferred using `typeOfExpr`
   - `assignVarType` checks for type conflicts
   - `checkMultipleType` validates explicit type annotations

2. **Variable Usage**: When an `ExprVar` is encountered:
   - The variable is looked up in the current scope
   - If not found, an `UndefinedVar` error is generated

3. **Scope Isolation**: Variables declared in nested blocks (if statements, loops) don't leak to outer scopes.

### Type Resolution

The `typeOfExpr` function determines expression types:
- Literals have fixed types (i32, f32, string, u8, bool, null)
- Variables get types from the scope
- Function calls get return types from function signatures
- Struct initializations get custom types

## Testing

The test suite in `test/Semantics/VarsSpec.hs` contains comprehensive examples of all error cases and valid programs to ensure the semantic analyzer works correctly.