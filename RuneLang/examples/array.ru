def show_array(arr: any[]) -> null
{
    for item in arr {
        show(item);
        show(' ');
    }
    show('\n');
}

def array_string() -> null
{
    arr: string[] = ["malloc", "sizeof", "char", "etoile"];

    arr[2] = "rune";
    show_array(arr);
}

def array_char() -> null
{
    arr: char[] = ['R', 'u', 'n', 'e'];

    show_array(arr);
}

def array_i32() -> i32[]
{
    arr: i32[] = [1, 2, 3, 4, 5];

    arr[1] = 10;
    arr
}

def main() -> null
{
    arr = array_i32();
    show_array(arr);

    array_string();
    array_char();
}
