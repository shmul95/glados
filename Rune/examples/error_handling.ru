def maybe_function() ~> i32
{
    num = 42;

    if num == 42 {
        return num;
    }
    return error("An error occurred");
}

def main() -> i32
{
    a = maybe_function()?;

    return 0;
}
