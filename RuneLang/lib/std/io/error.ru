somewhere
{
    def dprintf(fd: i32, fmt: string, args: ...any) -> i32;
    def write(count: u64, fd: i32, buf: *any, size: u64) -> i64;
}

/**
* public
*/

export def errorfmt(fmt: string, args: ...any) -> i32
{
    stderr = 2;

    dprintf(stderr, fmt, ...args)
}

export def error(b: bool) -> i32
{
    if b {
        errorfmt("true")
    }
    errorfmt("false")
}

export def error(i: i8) -> i32
{
    errorfmt("%hhd", i)
}

export def error(i: i16) -> i32
{
    errorfmt("%hd", i)
}

export def error(i: i32) -> i32
{
    errorfmt("%d", i)
}

export def error(i: i64) -> i32
{
    errorfmt("%ld", i)
}

export def error(u: u8) -> i32
{
    errorfmt("%hhu", u)
}

export def error(u: u16) -> i32
{
    errorfmt("%hu", u)
}

export def error(u: u32) -> i32
{
    errorfmt("%u", u)
}

export def error(u: u64) -> i32
{
    errorfmt("%lu", u)
}

export def error(s: string) -> i32
{
    errorfmt("%s", s)
}

export def error(f: f32) -> i32
{
    errorfmt("%f", f)
}

export def error(f: f64) -> i32
{
    errorfmt("%lf", f)
}

export def error(p: *any) -> i32
{
    errorfmt("%p", p)
}

export def error(c: char) -> i32
{
    putchar(c)
}
