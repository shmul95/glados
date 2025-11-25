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

def main() -> i32
{
    fibonacci(10)
}
