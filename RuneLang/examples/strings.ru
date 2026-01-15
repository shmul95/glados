somewhere { def show(c: char) -> null; }

def show_string(str: string) -> null
{
    for c in str {
        show(c);
    }
}

def get_string() -> string
{
    return "Hello, Rune!\n";
}

def main() -> null
{
    str: string = get_string();

    show_string(str);
}
