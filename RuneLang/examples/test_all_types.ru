def assert(actual: bool, expected: bool) -> i32
{
    if actual == expected {
        return 0;
    }
    return 1;
}

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

def main() -> i32
{
    ai8 : i8 = 42;
    bi8 : i8 = 10;
    ai16 : i16 = 1000;
    bi16 : i16 = 500;
    ai32 : i32 = 100000;
    bi32 : i32 = 50000;
    ai64 : i64 = 1000000;
    bi64 : i64 = 500000;
    au8 : u8 = 200;
    bu8 : u8 = 100;
    au16 : u16 = 50000;
    bu16 : u16 = 30000;
    au32 : u32 = 3000000;
    bu32 : u32 = 2000000;
    au64 : u64 = 9000000;
    bu64 : u64 = 8000000;
    achar: char = 'z';
    bchar: char = 'a';
    abool: bool = true;
    bbool: bool = false;
    rv: i32 = 0;

    rv += test_i8(ai8, bi8);
    rv += test_i16(ai16, bi16);
    rv += test_i32(ai32, bi32);
    rv += test_i64(ai64, bi64);
    rv += test_u8(au8, bu8);
    rv += test_u16(au16, bu16);
    rv += test_u32(au32, bu32);
    rv += test_u64(au64, bu64);
    rv += test_char(achar, bchar);
    rv += test_bool(abool, bbool);
    rv += test_and(abool, bbool);
    rv += test_or(abool, bbool);

    return rv;
}
