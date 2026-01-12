somewhere
{
    def printf(fmt: string, args: ...any) -> i32;
}

/**
* public
*/

export def showfmt(fmt: string, args: ...any) -> i32
{
    printf(fmt, args)
}
