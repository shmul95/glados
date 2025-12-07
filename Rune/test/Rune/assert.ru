def assert(expression: bool, expected: bool) -> i32
{
    if expression == expected {
        return 0;
    }
    show("ASSERTION FAILED\n");
    return 1;
}
