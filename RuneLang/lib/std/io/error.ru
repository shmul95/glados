somewhere
{
    extern def dprintf(fd: i32, format: string, args: ...any) -> null;
}

/**
* public
*/

export def error(v: string) -> null
{
    dprintf(2, "%s", v);
}

export def error(v: i8) -> null
{
    dprintf(2, "%hhd", v);
}

export def error(v: i16) -> null
{
    dprintf(2, "%hd", v);
}

export def error(v: i32) -> null
{
    dprintf(2, "%d", v);
}

export def error(v: i64) -> null
{
    dprintf(2, "%lld", v);
}

export def error(v: u8) -> null
{
    dprintf(2, "%hhu", v);
}

export def error(v: u16) -> null
{
    dprintf(2, "%hu", v);
}

export def error(v: u32) -> null
{
    dprintf(2, "%u", v);
}

export def error(v: u64) -> null
{
    dprintf(2, "%llu", v);
}

export def error(v: f32) -> null
{
    dprintf(2, "%f", v: f64);
}

export def error(v: f64) -> null
{
    dprintf(2, "%lf", v);
}

export def error(v: bool) -> null
{
    if v {
        error("true")
    } else {
        error("false")
    }
}

export def error(v: char) -> null
{
    dprintf(2, "%c", v);
}

export def error(v: *any) -> null
{
    if not v {
        error("(null)")
    }
    dprintf(2, "%p", v);
}
