somewhere { def assert(condition: bool, message: string) -> bool; }

/**
* public
*/

export def test_pointers() -> null
{
    assert(test_basic_reference_deref() == 42, "Pointers: basic reference and dereference");
    assert(test_pointer_modify() == 15, "Pointers: modify value through pointer");
    assert(test_pointer_arithmetic() == 53, "Pointers: pointer arithmetic");
    assert(test_nested_deref() == 107, "Pointers: nested pointer operations");
    assert(test_float_pointer() == 1, "Pointers: float pointer operations");
}

/**
* private
*/

def test_basic_reference_deref() -> i32
{
    a: i32 = 42;
    p: *i32 = &a;
    *p
}

def modify_int(v: *i32) -> null
{
    *v = *v + 10;
}

def test_pointer_modify() -> i32
{
    a: i32 = 5;
    modify_int(&a);
    a
}

def add_to_value(v: *i32, amount: i32) -> null
{
    *v = *v + amount;
}

def test_pointer_arithmetic() -> i32
{
    a: i32 = 10;
    add_to_value(&a, 5);
    add_to_value(&a, 8);
    add_to_value(&a, 30);
    a
}

def double_it(v: *i32) -> null
{
    *v = *v * 2;
}

def increment(v: *i32) -> null
{
    *v = *v + 1;
}

def test_nested_deref() -> i32
{
    x: i32 = 12;
    increment(&x);
    double_it(&x);
    double_it(&x);
    increment(&x);
    double_it(&x);
    increment(&x);
    x
}

def modify_float(v: *f32) -> null
{
    *v = *v + 1.0;
}

def test_float_pointer() -> i32
{
    a: f32 = 42.0 as f32;
    modify_float(&a);
    result: i32 = 0;
    if a > 42.5 as f32 {
        result = 1;
    }
    result
}
