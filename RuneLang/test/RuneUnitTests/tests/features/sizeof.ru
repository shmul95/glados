somewhere { def assert(condition: bool, message: string) -> bool; }

/**
* public
*/

export def test_sizeof() -> null
{
    test_sizeof_primitives();
    test_sizeof_variables();
    test_sizeof_immediate();
}

/**
* private
*/

def test_sizeof_primitives() -> null
{
    assert(sizeof i8 == 1, "Sizeof: primitive i8");
    assert(sizeof i16 == 2, "Sizeof: primitive i16");
    assert(sizeof i32 == 4, "Sizeof: primitive i32");
    assert(sizeof i64 == 8, "Sizeof: primitive i64");

    assert(sizeof u8 == 1, "Sizeof: primitive u8");
    assert(sizeof u16 == 2, "Sizeof: primitive u16");
    assert(sizeof u32 == 4, "Sizeof: primitive u32");
    assert(sizeof u64 == 8, "Sizeof: primitive u64");

    assert(sizeof f32 == 4, "Sizeof: primitive f32");
    assert(sizeof f64 == 8, "Sizeof: primitive f64");

    assert(sizeof bool == 1, "Sizeof: primitive bool");
    assert(sizeof char == 1, "Sizeof: primitive char");
    assert(sizeof string == 8, "Sizeof: primitive string");
    assert(sizeof *any == 8, "Sizeof: primitive pointer");
}

def test_sizeof_variables() -> null
{
    v_i8: i8 = 0;
    v_i64: i64 = 0;
    v_u32: u32 = 0;
    v_f32: f32 = 0.0;
    v_ptr: *any = null;
    v_str: string = "test";
    v_char: char = 'R';
    v_bool: bool = true;

    assert(sizeof v_i8 == 1, "Sizeof: variable i8");
    assert(sizeof v_i64 == 8, "Sizeof: variable i64");
    assert(sizeof v_u32 == 4, "Sizeof: variable u32");
    assert(sizeof v_f32 == 4, "Sizeof: variable f32");
    assert(sizeof v_ptr == 8, "Sizeof: variable pointer");
    assert(sizeof v_str == 8, "Sizeof: variable string");
    assert(sizeof v_char == 1, "Sizeof: variable char");
    assert(sizeof v_bool == 1, "Sizeof: variable bool");
}

def test_sizeof_immediate() -> null
{
    assert(sizeof 10: i8 == 1, "Sizeof: immediate i8");
    assert(sizeof 10: i64 == 8, "Sizeof: immediate i64");
    assert(sizeof 3.14: f32 == 4, "Sizeof: immediate f32");
    assert(sizeof 'A' == 1, "Sizeof: immediate char");
    assert(sizeof true == 1, "Sizeof: immediate bool");
    assert(sizeof null == 8, "Sizeof: immediate null");
    assert(sizeof "rune" == 8, "Sizeof: immediate string literal");
    assert(sizeof [1, 2, 3] == 4 * 3, "Sizeof: immediate array literal");
    assert(sizeof ["a", "b"] == 8 * 2, "Sizeof: immediate string array literal");
    assert(sizeof ['a', 'b'] == 1 * 2, "Sizeof: immediate char array literal");
    assert(sizeof (10: i32 + 5: i32) == 4, "Sizeof: immediate expression i32");
    assert(sizeof (10: i64 * 2: i64) == 8, "Sizeof: immediate expression i64");
}
