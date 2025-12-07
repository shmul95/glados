def test_i8(a:i8, b:i8) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_i16(a:i16, b:i16) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_i32(a:i32, b:i32) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_i64(a:i64, b:i64) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_u8(a:u8, b:u8) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_u16(a:u16, b:u16) -> i32
{
    rv: i32 = 0;
    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_u32(a:u32, b:u32) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_u64(a:u64, b:u64) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_char(a:char, b:char) -> i32
{
    rv: i32 = 0;

    rv += assert(a > b, true);
    rv += assert(a < b, false);
    rv += assert(a == b, false);
    return rv;
}

def test_bool(a:bool, b:bool) -> i32
{
    rv: i32 = 0;

    rv += assert(a == a, true);
    rv += assert(a == b, false);
    rv += assert(b == b, true);
    return rv;
}

def test_and(a:bool, b:bool) -> i32
{
    rv: i32 = 0;

    rv += assert(a && a, true);
    rv += assert(a && b, false);
    rv += assert(b && b, false);
    return rv;
}

def test_or(a:bool, b:bool) -> i32
{
    rv: i32 = 0;

    rv += assert(a || a, true);
    rv += assert(a || b, true);
    rv += assert(b || b, false);
    return rv;
}

def test_compares() -> i32
{
    rv: i32 = 0;

    rv += test_i8(10, 5);
    rv += test_i16(1000, 500);
    rv += test_i32(100000, 50000);
    rv += test_i64(10000000000, 5000000000);
    rv += test_u8(10, 5);
    rv += test_u16(1000, 500);
    rv += test_u32(100000, 50000);
    rv += test_u64(10000000000, 5000000000);
    rv += test_char('z', 'a');
    rv += test_bool(true, false);
    rv += test_and(true, false);
    rv += test_or(true, false);

    return rv;
}
