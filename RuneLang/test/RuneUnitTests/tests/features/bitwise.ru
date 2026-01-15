somewhere
{
    def assert(condition: bool, message: string) -> null;
}

/**
* public
*/

export def test_bitwise() -> null
{
    test_bit_and();
    test_bit_not();
}

/**
* private
*/

def test_bit_and() -> null
{
    // 5: 0101, 3: 0011 -> 5 & 3 = 1 (0001)
    a = 5: i32;
    b = 3: i32;

    assert((a & b) == 1, "Bitwise: AND i32");
}

def test_bit_not() -> null
{
    // ~0: i32 should be -1
    assert(~(0: i32) == -1, "Bitwise: NOT i32");
}
