somewhere { def assert(condition: bool, message: string) -> bool; }

/**
* public
*/

export def test_loops() -> null
{
    assert(loops() == 87, "Loops: loop {}");
    assert(for_to() == 695, "Loops: for ... to ...");
    assert(for_in() == 15, "Loops: for ... in ...");
}

/**
* private
*/

def loops() -> i32
{
    total = 0;
    i = 0;

    loop {
        i += 1;
        
        if i > 10 {
            stop;
        }

        if i % 2 == 0 {
            next;
        }

        j = 0;
        loop {
            j += 1;
            
            if j > i {
                stop;
            }

            if j == 2 {
                next;
            }

            total += j;
        }
    }
    total
}

def for_to() -> i32
{
    total = 0;
    for i = 1 to 10 {
        total += i;
        ++i;
    }

    for i = 0 to -50 {
        i -= 2;
        total -= i;
    }
    total
}

def for_in() -> i32
{
    total = 0;
    for i in [1, 2, 3, 4, 5] {
        total += i;
    }
    total
}
