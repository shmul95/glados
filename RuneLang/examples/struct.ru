def dummy_func() -> i32
{
    return 42;
}

override def dummy_func(i: i32) -> i32
{
    return i + 1;
}

def main() -> null
{
    v = dummy_func(42);

    show(v);
}