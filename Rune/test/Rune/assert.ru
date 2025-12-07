/*
* TODO: in the future, override the assert function
*
* make assert signature like:
* def assert(expression: any, expected: any) -> i32
*/
def assert(expression: bool, expected: bool) -> i32
{
    if expression == expected {
        return 0;
    }
    return 1;
}
