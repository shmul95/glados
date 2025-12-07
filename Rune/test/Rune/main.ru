def main() -> i32
{
    show("[*] starting test run...\n");
    show('\n');

    total_failures: i32 = test_compares();

    show('\n');

    if total_failures == 0 {
        show("[+] all tests passed successfully!\n");
        return 0;
    }

    show("[-] some tests failed. total failures: ");
    show(total_failures);
    show('\n');

    return 1;
}
