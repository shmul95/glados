somewhere { def assert(condition: bool, message: string) -> bool; }

/**
* public
*/

export def test_operators() -> null
{
    operators(42: i8, 10: i8);
    operators(1000: i16, 500: i16);
    operators(100000: i32, 50000: i32);
    operators(1000000: i64, 500000: i64);

    operators(200: u8, 100: u8);
    operators(50000: u16, 30000: u16);
    operators(3000000: u32, 2000000: u32);
    operators(9000000: u64, 8000000: u64);

    operators(3.14: f32, 1.59: f32);
    operators(2.7182818284: f64, 1.6180339887: f64);

    operators('z', 'a');

    logical(true, false);
}

/**
* private
*/

def operators(a: any, b: any) -> null
{
    assert(a > b, "Operators: a > b");
    assert(!(a < b), "Operators: a < b");
    assert(not(a == b), "Operators: a == b");
}

def logical(a: bool, b: bool) -> null
{
    assert(a && a, "Operators: true && true");
    assert(!(a and b), "Operators: true && false");
    assert(not(b && b), "Operators: false && false");

    assert(a or a, "Operators: true || true");
    assert(a || b, "Operators: true || false");
    assert(not(b || b), "Operators: false || false");
}

