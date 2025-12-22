def show_array(array: any[]) -> null
{
    comma = true;

    show('[');
    for element in array {
        if not comma {
            show(", ");
        }
        show(element);
        comma = false;
    }
    show("]\n");
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

def array_of_array() -> null
{
    kurwa: i32[][] = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

    for bobr in kurwa {
        show_array(bobr);
    }
}

def main() -> null
{
    arr = array_i32();
    show_array(arr);

    array_string();
    array_char();
    array_of_array();
}
