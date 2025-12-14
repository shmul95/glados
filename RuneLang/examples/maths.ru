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

def main() -> null
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
