somewhere
{ 
    def assert(condition: bool, message: string) -> bool;
    def error(s: string) -> null;
}

/**
* public
*/

export def test_errors() -> null
{
    test_unwrap_success();
    test_error_propagation();
    test_nested_error_halt();
}

/**
* private
*/

def fail_func() ~> i32
{
    if true {
        return error("error");
    }
    return 1;
}

def succeed_func(n: i32) ~> i32
{
    return n;
}

def workflow(tracker: []i32) ~> i32
{
    tracker[0] = 1;
    val = fail_func()?;
    tracker[0] = 2;
    return val;
}

def test_unwrap_success() -> null
{
    val = succeed_func(42)?;
    assert(val == 42, "Error test: unwrap success");
}

def test_error_propagation() -> null
{
    tracker: []i32 = [0];
    _ = workflow(tracker);

    assert(tracker[0] == 1, "Error test: execution halted");
    assert(tracker[0] != 2, "Error test: bypass check");
}

def level_two(tracker: []i32) ~> i32
{
    tracker[0] = 10;
    _ = fail_func()?;
    tracker[0] = 20;
    return 0;
}

def level_one(tracker: []i32) ~> i32
{
    _ = level_two(tracker)?;
    tracker[0] = 30;
    return 0;
}

def test_nested_error_halt() -> null
{
    tracker: []i32 = [0];
    _ = level_one(tracker);

    assert(tracker[0] == 10, "Error test: nested propagation halt");
    assert(tracker[0] != 20, "Error test: nested level two bypass");
    assert(tracker[0] != 30, "Error test: nested level one bypass");
}
