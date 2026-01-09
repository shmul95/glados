/**
* public
*/

export def assert(condition: bool, message: string) -> bool
{
    if not condition {
        errorln(message);
        false
    }

    showln(message);
    true
}

/**
* private
*/

def errorln(message: string) -> null
{
    error("[\033[31m✗\033[0m]\t");
    error(message);
    error('\n')
}

def showln(message: string) -> null
{
    show("[\033[32m✓\033[0m]\t");
    show(message);
    show('\n')
}
