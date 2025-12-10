<h1 align="center">
    Rune
</h1>
<h2 align="center">
    Mid-level natively compiled programming language
</h2>

<div align="center">
  <img src="https://raw.githubusercontent.com/catppuccin/catppuccin/main/assets/palette/macchiato.png" width="600px"/>
  <p></p>
  <div align="center">
     <a href="https://github.com/shmul95/glados/stargazers">
        <img src="https://img.shields.io/github/stars/shmul95/glados?color=F5BDE6&labelColor=303446&style=for-the-badge&logo=starship&logoColor=F5BDE6">
     </a>
     <a href="https://github.com/shmul95/glados/">
        <img src="https://img.shields.io/github/repo-size/shmul95/glados?color=C6A0F6&labelColor=303446&style=for-the-badge&logo=github&logoColor=C6A0F6">
     </a>
     <a href="https://github.com/shmul95/glados/blob/main/LICENSE">
        <img src="https://img.shields.io/static/v1.svg?style=for-the-badge&label=License&message=BSD-3-Clause&colorA=313244&colorB=F5A97F&logo=unlicense&logoColor=F5A97F&"/>
     </a>
  </div>
  <br>
</div>

## Description

Rune is a **mid-level**, compiled programming language. It supports **static typing** with optional **type inference**, first-class functions, structs, methods, override and error handling.
<br>
The philosophy of the language  is to code like in a **high level language** (*Ruby*, *Crystal*, *Python*) with **low-level** performance (*C*, *C++*)


## Basic Syntax

### Comments

```js
// single-line comment

/*
multi-line comments
*/
```

## Statements

- Variable declaration
- Function calls
- Struct initialization
- Control flow statements
- Return statements

## Types

Rune supports primitive and composite types:

`i32` -> 32-bit integer
`f32` -> 32-bit floating point
`bool` -> Boolean (`true`/`false`)
`u8` -> single char
`string` -> UTF-8 string
`any` -> dynamic type, can store any type, for inference
`null` -> no return value, like None, void ect...

### Type Inference

```c
num: i32 = 0; //explicit type
num = 0; //type inferred as i32
```

> [!NOTE]
> Function parameters and return types **cannot** be inferred; they must be explicitly declared.

## Functions

Functions are declared using `def` keyword. Return types **must** be explicitly defined.

```rb
def add(a: i32, b: i32) -> i32
{
    a + b
}
```
- **Implicit return:** the last expression is returned automatically if no `;`.
- **Explicit return:** Use `return` keyword with a `;`.
- **Error propagation operator:** `?` propagates errors.

```rb
def maybe_value() ~> i32
{
    val = 42;

    if val == 42 {
        return val;
    }
    return error("error msg");
}

def main() -> i32
{
    a = maybe_value()?;
    0
}
```
- **->** operator defines the return value.
- **~>** operator defines the `expected` return value

## Structs and Methods

Structs are used to define custom data types.

```crystal
struct Vec2f
{
    x: f32;
    y: f32;

    def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }
}
```

- Methods are defined inside structs.
- `self` refers to the instance.

### Override Built-in Functions:

```ts
override def show(v: Vec2f) -> null
{
    show("Vec2f(x: ");
    show(v.x);
    show(", y: ");
    show(v.y);
}
```
-> creates an instance of `show()` for Vec2f

## Control Flow

### If / Else

```go
if condition {
    /* do something */
} else {
    /* do else */
}
```

### Loops

```rust
for i = 0 to 10 {
    ++i;
}
```

### Operators

`+`, `-`, `*`, `/` -> Arithmetic
<br>
`+=`, `-=`, `*=`, `/=`, `%=` -> Arithmetic assignment
<br>
`%` -> Modulus
<br>
`==`, `!=`, `<`, `<=`, `>`, `>=` -> Comparison
<br>
`&&`, `||` -> Logical
<br>
`=` -> Assignement

## Built-in Functions

- `def show(value: any) -> null` prints value to **stdout**
- `def error(value: any) -> null` prints value to **stderr**

Example:
```rust
show("Hello Rune\n");
error("Something went wrong\n");
```

## Error Handling

- Errors are returned using `error(...)`.
- The `?` operator propagates errors up the call stack.

```rust
result = maybe_function()?;
```

## Examples

### Hello Rune

```crystal
def main() -> null
{
    show("Hello, World!\n");
}
```

### Fibonacci

```crystal
def fibonacci(n: i32) -> i32
{
    if n <= 0 {
        0
    }
    if n == 1 {
        1
    }
    fibonacci(n - 1) + fibonacci(n - 2)
}
```

### Structs

```crystal
struct Vec2f
{
    x: f32;
    y: f32;

    def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }

}

override def show(v: Vec2f) -> null
{
    show("Vec2f(x: ");
    show(v.x);
    show(", y: ");
    show(v.y);
}

def main() -> null
{
    a = Vec2f { x: 1.0, y: 2.0 };
    b = Vec2f { x: 3.0, y: 4.0 };
    c = a.add(b);

    show(c);
}
```

for additional examples, click [**here**](./examples/)
