/*
* def add(a: any, b: any) -> any
* {
*     a + b
* }
*/

def add(a: i8, b: i8) -> i8
{
    a + b
}

override def add(a: i16, b: i16) -> i16
{
    a + b
}

override def add(a: i32, b: i32) -> i32
{
    a + b
}

override def add(a: i64, b: i64) -> i64
{
    a + b
}

override def add(a: f32, b: f32) -> f32
{
    a + b
}

override def add(a: f64, b: f64) -> f64
{
    a + b
}

override def add(a: u8, b: u8) -> u8
{
    a + b
}

override def add(a: u16, b: u16) -> u16
{
    a + b
}

override def add(a: u32, b: u32) -> u32
{
    a + b
}

override def add(a: u64, b: u64) -> u64
{
    a + b
}

def assert(condition: bool, message: string) -> null
{
    if (condition == false) {
        show("[-] FAILED\t");
        show(message);
        show('\n');
        return;
    }
    show("[+] PASSED");
    show('\n');
}

def test_addition() -> null
{
    result_i8:  i8  = add(10, 20);
    result_i16: i16 = add(300, 400);
    result_i32: i32 = add(1000, 2000);
    result_i64: i64 = add(100000, 200000);
    result_f32: f32 = add(1.5, 2.5);
    result_f64: f64 = add(2.5, 3.5);
    result_u8:  u8  = add(10, 20);
    result_u16: u16 = add(300, 400);
    result_u32: u32 = add(1000, 2000);
    result_u64: u64 = add(100000, 200000);

    assert(result_i8  == 30,     "i8 addition failed");
    assert(result_i16 == 700,    "i16 addition failed");
    assert(result_i32 == 3000,   "i32 addition failed");
    assert(result_i64 == 300000, "i64 addition failed");
    assert(result_f32 == 4.0,    "f32 addition failed");
    assert(result_f64 == 6.0,    "f64 addition failed");
    assert(result_u8  == 30,     "u8 addition failed");
    assert(result_u16 == 700,    "u16 addition failed");
    assert(result_u32 == 3000,   "u32 addition failed");
    assert(result_u64 == 300000, "u64 addition failed");
}

def test_subtraction() -> null
{
    result_i8:  i8  = 20 - 10;
    result_i16: i16 = 400 - 300;
    result_i32: i32 = 2000 - 1000;
    result_i64: i64 = 200000 - 100000;
    result_f32: f32 = 2.5 - 1.5;
    result_f64: f64 = 3.5 - 2.5;
    result_u8:  u8  = 20 - 10;
    result_u16: u16 = 400 - 300;
    result_u32: u32 = 2000 - 1000;
    result_u64: u64 = 200000 - 100000;

    assert(result_i8  == 10,     "i8 subtraction failed");
    assert(result_i16 == 100,    "i16 subtraction failed");
    assert(result_i32 == 1000,   "i32 subtraction failed");
    assert(result_i64 == 100000, "i64 subtraction failed");
    assert(result_f32 == 1.0,    "f32 subtraction failed");
    assert(result_f64 == 1.0,    "f64 subtraction failed");
    assert(result_u8  == 10,     "u8 subtraction failed");
    assert(result_u16 == 100,    "u16 subtraction failed");
    assert(result_u32 == 1000,   "u32 subtraction failed");
    assert(result_u64 == 100000, "u64 subtraction failed");
}

def test_multiplication() -> null
{
    result_i8:  i8  = 10 * 20;
    result_i16: i16 = 300 * 400;
    result_i32: i32 = 1000 * 2000;
    result_i64: i64 = 100000 * 200000;
    result_f32: f32 = 1.5 * 2.5;
    result_f64: f64 = 2.5 * 3.5;
    result_u8:  u8  = 10 * 20;
    result_u16: u16 = 300 * 400;
    result_u32: u32 = 1000 * 2000;
    result_u64: u64 = 100000 * 200000;

    assert(result_i8  == 200,         "i8 multiplication failed");
    assert(result_i16 == 120000,      "i16 multiplication failed");
    assert(result_i32 == 2000000,     "i32 multiplication failed");
    assert(result_i64 == 20000000000, "i64 multiplication failed");
    assert(result_f32 == 3.75,        "f32 multiplication failed");
    assert(result_f64 == 8.75,        "f64 multiplication failed");
    assert(result_u8  == 200,         "u8 multiplication failed");
    assert(result_u16 == 120000,      "u16 multiplication failed");
    assert(result_u32 == 2000000,     "u32 multiplication failed");
    assert(result_u64 == 20000000000, "u64 multiplication failed");
}

def test_division() -> null
{
    result_i8:  i8  = 20 / 10;
    result_i16: i16 = 400 / 300;
    result_i32: i32 = 2000 / 1000;
    result_i64: i64 = 200000 / 100000;
    result_f32: f32 = 2.5 / 1.5;
    result_f64: f64 = 3.5 / 2.5;
    result_u8:  u8  = 20 / 10;
    result_u16: u16 = 400 / 300;
    result_u32: u32 = 2000 / 1000;
    result_u64: u64 = 200000 / 100000;

    assert(result_i8  == 2,      "i8 division failed");
    assert(result_i16 == 1,      "i16 division failed");
    assert(result_i32 == 2,      "i32 division failed");
    assert(result_i64 == 2,      "i64 division failed");
    assert(result_f32 == 1.6666666, "f32 division failed");
    assert(result_f64 == 1.4,    "f64 division failed");
    assert(result_u8  == 2,      "u8 division failed");
    assert(result_u16 == 1,      "u16 division failed");
    assert(result_u32 == 2,      "u32 division failed");
    assert(result_u64 == 2,      "u64 division failed");
}

def test_modulo() -> null
{
    result_i8:  i8  = 20 % 6;
    result_i16: i16 = 400 % 300;
    result_i32: i32 = 2000 % 1000;
    result_i64: i64 = 200000 % 100000;
    result_u8:  u8  = 20 % 6;
    result_u16: u16 = 400 % 300;
    result_u32: u32 = 2000 % 1000;
    result_u64: u64 = 200000 % 100000;

    assert(result_i8  == 2,      "i8 modulo failed");
    assert(result_i16 == 100,    "i16 modulo failed");
    assert(result_i32 == 0,      "i32 modulo failed");
    assert(result_i64 == 0,      "i64 modulo failed");
    assert(result_u8  == 2,      "u8 modulo failed");
    assert(result_u16 == 100,    "u16 modulo failed");
    assert(result_u32 == 0,      "u32 modulo failed");
    assert(result_u64 == 0,      "u64 modulo failed");
}

def test_operation_order() -> null
{
    result: i32 = 10 + 20 * 3 - 5 / 5;
    result2: i32 = (10 + (20 * 3)) - (5 / 5);
    result3: i32 = (10 + 20) * (3 - 5) / 5;

    assert(result == 69, "Operation order failed");
    assert(result2 == 69, "Operation order with parentheses failed");
    assert(result3 == -12, "Operation order with different parentheses failed");
}

def main() -> null
{
    test_addition();
    test_subtraction();
    test_multiplication();
    test_division();
    test_modulo();
    test_operation_order();
}
