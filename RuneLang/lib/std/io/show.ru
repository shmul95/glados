somewhere
{
    extern def printf(format: string, args: ...any) -> i32;
    extern def putchar(c: char) -> i32;
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
