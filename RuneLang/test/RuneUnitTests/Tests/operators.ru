somewhere { def assert(condition: bool, message: string) -> null; }

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
    assert(a > b, "Failure: a > b");
    assert(!(a < b), "Failure: a < b should be false");
    assert(not(a == b), "Failure: a == b should be false");
}

def logical(a: bool, b: bool) -> null
{
    assert(a && a, "Failure: true && true");
    assert(!(a and b), "Failure: true && false should be false");
    assert(not(b && b), "Failure: false && false should be false");

    assert(a or a, "Failure: true || true");
    assert(a || b, "Failure: true || false");
    assert(not(b || b), "Failure: false || false should be false");
}

