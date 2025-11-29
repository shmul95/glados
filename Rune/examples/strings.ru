def show_string(str: string) -> null
{
    for c in str {
        show(c);
    }
}

def main() -> null
{
    str: string = "Hello, Rune!\n";

    show_string(str);
}
