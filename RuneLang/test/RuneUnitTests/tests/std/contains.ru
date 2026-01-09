somewhere
{ 
    def assert(condition: bool, message: string) -> bool; 
    def contains(s: string, sub: string) -> bool;
}

/**
* public
*/

export def test_contains() -> null
{
    assert(contains("hello", "ell"), "Contains: middle match");
    assert(contains("hello", "hell"), "Contains: start match");
    assert(contains("hello", "lo"), "Contains: end match");
    assert(contains("hello", "hello"), "Contains: full match");
    
    assert(contains("hello", ""), "Contains: empty sub");
    assert(contains("", ""), "Contains: empty in empty");
    
    assert(not(contains("hello", "world")), "Contains: no match");
    assert(not(contains("hello", "hello world")), "Contains: sub too long");
    assert(not(contains("", "a")), "Contains: non-empty in empty");
    
    assert(contains("aaaaa", "aa"), "Contains: overlapping sequence");
    assert(contains("abcabcabc", "cabc"), "Contains: repeating patterns");
    
    assert(not(contains("hello", "H")), "Contains: case sensitivity");
    assert(contains("banana", "ana"), "Contains: multiple occurrences");
    assert(contains("123456789", "89"), "Contains: near end of long string");
}
