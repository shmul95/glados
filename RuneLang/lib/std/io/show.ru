somewhere
{
    def showfmt(fmt: string, args: ...any) -> i32;
    def putchar(c: char) -> i32;
}

/**
* public
*/

export def showfmt(fmt: string, args: ...any) -> i32
{
    showfmt(fmt, args)
}

export def show(b: bool) -> i32
{
    if b {
        showfmt("true")
    }
    showfmt("false")
}

export def show(i: i8) -> i32
{
    showfmt("%hhd", i)
}

export def show(i: i16) -> i32
{
    showfmt("%hd", i)
}

export def show(i: i32) -> i32
{
    showfmt("%d", i)
}

export def show(i: i64) -> i32
{
    showfmt("%ld", i)
}

export def show(u: u8) -> i32
{
    showfmt("%hhu", u)
}

export def show(u: u16) -> i32
{
    showfmt("%hu", u)
}

export def show(u: u32) -> i32
{
    showfmt("%u", u)
}

export def show(u: u64) -> i32
{
    showfmt("%lu", u)
}

export def show(s: string) -> i32
{
    showfmt("%s", s)
}

export def show(f: f32) -> i32
{
    showfmt("%f", f)
}

export def show(f: f64) -> i32
{
    showfmt("%lf", f)
}

export def show(p: *any) -> i32
{
    showfmt("%p", p)
}

export def show(c: char) -> i32
{
    putchar(c)
}
