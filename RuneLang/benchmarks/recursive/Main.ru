def fib(n: i64) -> i64 {
    if n <= 1 {
        n
    }
    fib(n - 1) + fib(n - 2)
}

def main() -> null {
    show(fib(40));
}
