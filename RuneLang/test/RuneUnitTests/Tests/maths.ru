somewhere
{
    def assert(condition: bool, message: string) -> null;
}

/**
* public
*/

export def test_maths() -> null
{
    maths(10: i8, 3: i8, [13, 7, 30, 3, 1]);
    maths(10: i16, 3: i16, [13, 7, 30, 3, 1]);
    maths(10: i32, 3: i32, [13, 7, 30, 3, 1]);
    maths(10: i64, 3: i64, [13, 7, 30, 3, 1]);
    maths(10: u8, 3: u8, [13, 7, 30, 3, 1]);
    maths(10: u16, 3: u16, [13, 7, 30, 3, 1]);
    maths(10: u32, 3: u32, [13, 7, 30, 3, 1]);
    maths(10: u64, 3: u64, [13, 7, 30, 3, 1]);
}

/**
* private
*/

def maths(a: any, b: any, results: any[]) -> null
{
    assert(a + b == results[0], "Addition failed");
    assert(a - b == results[1], "Subtraction failed");
    assert(a * b == results[2], "Multiplication failed");
    assert(a / b == results[3], "Division failed");
    assert(a % b == results[4], "Modulus failed");
}
