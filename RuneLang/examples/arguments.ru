def showln(value: any) -> null
{
    show(value);
    show('\n');
}

def main(argc: i32, argv: string[], env: string[]) -> i32
{
    if argc < 2 {
        84
    }

    for param in argv {
        showln(param);
    }
    for variable in env {
        showln(variable);
    }
    0
}
