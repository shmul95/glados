/*
* TODO:
* - fix multiple-file test suite support and move this to test/Rune/assert.ru
* - override def assert(description: string, expression: bool, expected: bool) -> i32
*/
def log_assert(description: string, expression: bool, expected: bool) -> i32
{
    rv: i32 = assert(expression, expected);

    if rv == 0 {
        show("  [+] "); show(description); show('\n');
        return 0;
    }

    show("  [-] "); show(description); show('\n');
    show("    assertion failed - expected: ");
    show(expected); show(", actual: "); show(expression);
    show('\n');
    return 1;
}

def test_i8(a:i8, b:i8) -> i32
{
    rv: i32 = 0;
    show("[*] testing i8 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("i8: a > b", a > b, true);
    rv += log_assert("i8: a < b", a < b, false);
    rv += log_assert("i8: a == b", a == b, false);
    return rv;
}

def test_i16(a:i16, b:i16) -> i32
{
    rv: i32 = 0;
    show("[*] testing i16 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("i16: a > b", a > b, true);
    rv += log_assert("i16: a < b", a < b, false);
    rv += log_assert("i16: a == b", a == b, false);
    return rv;
}

def test_i32(a:i32, b:i32) -> i32
{
    rv: i32 = 0;
    show("[*] testing i32 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("i32: a > b", a > b, true);
    rv += log_assert("i32: a < b", a < b, false);
    rv += log_assert("i32: a == b", a == b, false);
    return rv;
}

def test_i64(a:i64, b:i64) -> i32
{
    rv: i32 = 0;
    show("[*] testing i64 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("i64: a > b", a > b, true);
    rv += log_assert("i64: a < b", a < b, false);
    rv += log_assert("i64: a == b", a == b, false);
    return rv;
}

def test_u8(a:u8, b:u8) -> i32
{
    rv: i32 = 0;
    show("[*] testing u8 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("u8: a > b", a > b, true);
    rv += log_assert("u8: a < b", a < b, false);
    rv += log_assert("u8: a == b", a == b, false);
    return rv;
}

def test_u16(a:u16, b:u16) -> i32
{
    rv: i32 = 0;
    show("[*] testing u16 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("u16: a > b", a > b, true);
    rv += log_assert("u16: a < b", a < b, false);
    rv += log_assert("u16: a == b", a == b, false);
    return rv;
}

def test_u32(a:u32, b:u32) -> i32
{
    rv: i32 = 0;
    show("[*] testing u32 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("u32: a > b", a > b, true);
    rv += log_assert("u32: a < b", a < b, false);
    rv += log_assert("u32: a == b", a == b, false);
    return rv;
}

def test_u64(a:u64, b:u64) -> i32
{
    rv: i32 = 0;
    show("[*] testing u64 ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("u64: a > b", a > b, true);
    rv += log_assert("u64: a < b", a < b, false);
    rv += log_assert("u64: a == b", a == b, false);
    return rv;
}

def test_char(a:char, b:char) -> i32
{
    rv: i32 = 0;
    show("[*] testing char ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("char: a > b", a > b, true);
    rv += log_assert("char: a < b", a < b, false);
    rv += log_assert("char: a == b", a == b, false);
    return rv;
}

def test_bool(a:bool, b:bool) -> i32
{
    rv: i32 = 0;
    show("[*] testing bool equality ("); show(a); show(" vs "); show(b); show(")\n");

    rv += log_assert("bool: a == a", a == a, true);
    rv += log_assert("bool: a == b", a == b, false);
    rv += log_assert("bool: b == b", b == b, true);
    return rv;
}

def test_and(a:bool, b:bool) -> i32
{
    rv: i32 = 0;
    show("[*] testing logical and ("); show(a); show(" && "); show(b); show(")\n");

    rv += log_assert("and: a && a", a && a, true);
    rv += log_assert("and: a && b", a && b, false);
    rv += log_assert("and: b && b", b && b, false);
    return rv;
}

def test_or(a:bool, b:bool) -> i32
{
    rv: i32 = 0;
    show("[*] testing logical or ("); show(a); show(" || "); show(b); show(")\n");

    rv += log_assert("or: a || a", a || a, true);
    rv += log_assert("or: a || b", a || b, true);
    rv += log_assert("or: b || b", b || b, false);
    return rv;
}

def test_compares() -> i32
{
    rv: i32 = 0;

    show("[*] starting comparisons test suite\n");

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

    show("[*] ending comparisons test suite\n");

    return rv;
}
