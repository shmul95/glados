somewhere
{
    def assert(condition: bool, message: string) -> bool; 
    def length(s: string) -> u64;
}

/**
* public
*/

export def test_length() -> null
{
    assert(length("") == 0, "Length: empty");
    assert(length("a") == 1, "Length: 1 char");
    assert(length("ab") == 2, "Length: 2 chars");
    assert(length("abcdefg") == 7, "Length: 7 chars (boundary - 1)");
    assert(length("abcdefgh") == 8, "Length: 8 chars (exact boundary)");
    assert(length("abcdefghi") == 9, "Length: 9 chars (boundary + 1)");
    assert(length("abcdefghijklmno") == 15, "Length: 15 chars");
    assert(length("abcdefghijklmnop") == 16, "Length: 16 chars (2 words)");
    assert(length("abcdefghijklmnopq") == 17, "Length: 17 chars");
    assert(length(" ") == 1, "Length: space");
    assert(length("long string with multiple words and spaces") == 42, "Length: long");
}
