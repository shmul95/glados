def errorln(message: string) -> null
{
    error("[-] FAILED\t");
    error(message);
    error('\n');
}

export def assert(condition: bool, message: string) -> null
{
    if not condition {
        errorln(message)
    }
    show("[+] PASSED\n")
}
