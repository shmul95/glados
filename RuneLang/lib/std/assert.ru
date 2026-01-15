somewhere
{
    def compare(str1: string, str2: string) -> i32;

    def error(v: string) -> i32;
    def error(v: char) -> i32;
    def show(v: string) -> i32;
    def show(v: char) -> i32;
}

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

export def assert_eq(str1: string, str2: string, message: string) -> bool
{
    assert(compare(str1, str2) == 0, message)
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
