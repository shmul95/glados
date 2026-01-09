somewhere
{ 
    def assert(condition: bool, message: string) -> bool; 
    def compare(a: string, b: string) -> i32;
}

/**
* public
*/

export def test_compare() -> null
{
    assert(compare("", "") == 0, "Compare: empty vs empty");
    assert(compare("a", "a") == 0, "Compare: single identical");
    assert(compare("abc", "abc") == 0, "Compare: identical string");
    
    assert(compare("a", "b") == -1, "Compare: a < b");
    assert(compare("b", "a") == 1, "Compare: b > a");
    
    assert(compare("abc", "abd") == -1, "Compare: last char smaller");
    assert(compare("abd", "abc") == 1, "Compare: last char larger");
    
    assert(compare("abc", "abcd") == -1, "Compare: prefix shorter");
    assert(compare("abcd", "abc") == 1, "Compare: prefix longer");
    
    assert(compare("", "a") == -1, "Compare: empty vs non-empty");
    assert(compare("a", "") == 1, "Compare: non-empty vs empty");

    assert(compare("abcde", "abxde") == -1, "Compare: middle difference");
    assert(compare("abxde", "abcde") == 1, "Compare: middle difference inverse");

    assert(compare("A", "a") == -1, "Compare: case sensitivity uppercase first");
    assert(compare("a", "A") == 1, "Compare: case sensitivity lowercase first");
}
