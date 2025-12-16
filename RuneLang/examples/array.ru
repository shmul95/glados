/*
override def show(arr: any[]) -> null
{
    for item in arr {
        show(item);
        show(' ');
    }
    show('\n');
}

def array_i32() -> null
{
    arr: i32[] = [1, 2, 3, 4, 5];

    arr[1] = 10;
    show(arr);
}

def array_string() -> null
{
    arr: string[] = ["malloc", "sizeof", "char", "etoile"];

    arr[2] = "rune";
    show(arr);
}

def array_char() -> null
{
    arr: char[] = ['R', 'u', 'n', 'e'];

    show(arr);
}

def main() -> null
{
    array_i32();
    array_string();
    array_char();
}
*/

def array_i32() -> null
{
    arr: i32[] = [1, 2, 3, 4, 5];

    arr[1] = 10;
    show(arr);
}

def main() -> null
{
    array_i32();
}
