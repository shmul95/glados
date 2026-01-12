somewhere
{
    def dprintf(fd: i32, fmt: string, args: ...any) -> i32;
}

/**
* public
*/

export def errorfmt(fmt: string, args: ...any) -> i32
{
    stderr = 2;

    dprintf(stderr, fmt, args)
}
