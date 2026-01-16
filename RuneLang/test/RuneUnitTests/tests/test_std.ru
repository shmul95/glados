somewhere
{
    /* std/string */
    def test_length() -> null;
    def test_compare() -> null;
    def test_contains() -> null;

    /* std/io */
    def test_file() -> null;
    def test_show() -> null;

    /* std/memory */
    def test_memory() -> null;
}

/**
* public
*/

export def test_std() -> null
{
    test_length();
    test_compare();
    test_contains();

    test_file();
    test_show();

    test_memory();
}
