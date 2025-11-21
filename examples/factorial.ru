def factorial(n: i32) ~> i32
{
    if n < 0 {
        return error("Cannot calculate factorial of negative number");
    }
    if n == 0 {
        1
    } else {
        res = factorial(n - 1)?;
        n * res
    }
}

def main() -> null
{
    val = factorial(5)?;
    show(val); 
}
