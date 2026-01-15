somewhere { def assert(condition: bool, message: string) -> bool; }

/**
* public
*/

export def test_arrays() -> null
{
    test_array_i32();
    test_array_char();
    test_array_nested();
}

/**
* private
*/

def test_array_i32() -> null
{
    arr: []i32 = [1, 2, 3, 4, 5];
    arr[1] = 10;
    
    assert(arr[0] == 1, "Array i32: Index 0 value");
    assert(arr[1] == 10, "Array i32: Index 1 modification");
    assert(arr[4] == 5, "Array i32: Index 4 value");
}

def test_array_char() -> null
{
    arr: []char = ['R', 'u', 'n', 'e'];
    
    assert(arr[0] == 'R', "Array char: Index 0");
    assert(arr[3] == 'e', "Array char: Index 3");
}

def test_array_nested() -> null
{
    kurwa: [][]i32 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

    assert(kurwa[0][0] == 1, "Nested array: [0][0]");
    assert(kurwa[1][1] == 5, "Nested array: [1][1]");
    assert(kurwa[2][2] == 9, "Nested array: [2][2]");

    sum = 0;
    for bobr in kurwa {
        for val in bobr {
            sum += val;
        }
    }
    assert(sum == 45, "Nested array: Iteration sum");
}
