somewhere
{
    def show(v: string) -> i32;
    def show(v: f32) -> i32;
    def show(v: *any) -> i32;

    def assert(condition: bool, message: string) -> bool;
}

/**
* public
*/

export def test_show() -> null
{
    test_show_string();
    test_show_f32();
    test_show_pointer();
    test_show_all();
}

/**
* private
*/

def test_show_string() -> null
{
    result = show("Hello, Rune!");

    assert(result > 0, "Show: string output");
}

def test_show_f32() -> null
{
    result = show(3.14159 as f32);

    assert(result > 0, "Show: f32 output");
}

def test_show_pointer() -> null
{
    ptr: *any = null;

    result = show(ptr);
    assert(result > 0, "Show: pointer output");

    ptr = &result as *any;
    assert(show(ptr) > 0, "Show: non-null pointer output");
}

def test_show_all() -> null
{
    str_result = show("Testing all types");
    f32_result = show(2.71828 as f32);
    ptr: *any = &str_result as *any;
    ptr_result = show(ptr);

    assert(str_result > 0, "Show: all types - string");
    assert(f32_result > 0, "Show: all types - f32");
    assert(ptr_result > 0, "Show: all types - pointer");
}
