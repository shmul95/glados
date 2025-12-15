def assert(condition: bool, message: string) -> null
{
    if (condition == false) {
        show("[-] FAILED\t");
        show(message);
        show('\n');
        return;
    }
    show("[+] PASSED");
    show('\n');
}

def logical_operators() -> null
{
    a = true;
    b = false;

    assert(a && a == true, "logical && failed");
    assert(a and b == false, "logical and failed");
    assert(a || b == true, "logical || failed");
    assert(a or b == true, "logical or failed");
    assert(!a == false, "logical ! failed");
    assert(not b == true, "logical not failed");
}

def main() -> null
{
    logical_operators();
}
