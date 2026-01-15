somewhere
{
    def dprintf(fd: i32, format: string, args: ...any) -> i32;
}

somewhere
{
    def printf(format: string, args: ...any) -> i32;
    def putchar(c: char) -> i32;
}

/**
* public
*/

export def show(v: string) -> i32
{
    printf("%s", v)
}

export def show(v: i8) -> i32
{
    printf("%hhd", v)
}

export def show(v: i16) -> i32
{
    printf("%hd", v)
}

export def show(v: i32) -> i32
{
    printf("%d", v)
}

export def show(v: i64) -> i32
{
    printf("%lld", v)
}

export def show(v: u8) -> i32
{
    printf("%hhu", v)
}

export def show(v: u16) -> i32
{
    printf("%hu", v)
}

export def show(v: u32) -> i32
{
    printf("%u", v)
}

export def show(v: u64) -> i32
{
    printf("%llu", v)
}

export def show(v: f32) -> i32
{
    printf("%f", v: f64)
}

export def show(v: f64) -> i32
{
    printf("%lf", v)
}

export def show(v: bool) -> i32
{
    if v {
        show("true")
    } else {
        show("false")
    }
}

export def show(v: char) -> i32
{
    putchar(v)
}

export def show(v: *any) -> i32
{
    if not v {
        show("(null)")
    }
    printf("%p", v)
}

/**
* public
*/

export def error(v: string) -> i32
{
    dprintf(2, "%s", v)
}

export def error(v: i8) -> i32
{
    dprintf(2, "%hhd", v)
}

export def error(v: i16) -> i32
{
    dprintf(2, "%hd", v)
}

export def error(v: i32) -> i32
{
    dprintf(2, "%d", v)
}

export def error(v: i64) -> i32
{
    dprintf(2, "%lld", v)
}

export def error(v: u8) -> i32
{
    dprintf(2, "%hhu", v)
}

export def error(v: u16) -> i32
{
    dprintf(2, "%hu", v)
}

export def error(v: u32) -> i32
{
    dprintf(2, "%u", v)
}

export def error(v: u64) -> i32
{
    dprintf(2, "%llu", v)
}

export def error(v: f32) -> i32
{
    dprintf(2, "%f", v: f64)
}

export def error(v: f64) -> i32
{
    dprintf(2, "%lf", v)
}

export def error(v: bool) -> i32
{
    if v {
        error("true")
    } else {
        error("false")
    }
}

export def error(v: char) -> i32
{
    dprintf(2, "%c", v)
}

export def error(v: *any) -> i32
{
    if not v {
        error("(null)")
    }
    dprintf(2, "%p", v)
}

def show(args: ...any) -> i32
{
    total: i32 = 0;

    for arg in args {
        total += show(arg);
    }
    total
}

def main() -> null
{
    show("Hello, World!\n", 42, '\n', 3.14: f32, '\n', true, '\n', null, '\n');
}
