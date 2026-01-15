def f() -> null
{
    return 0;
    return 1; //<< throw unreachable code 
}

def main() -> null
{
    f();
}