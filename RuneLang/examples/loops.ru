/**
TODO: override like this maybe

override def show(msg: string, value: i32) -> null
{
    show(msg);
    show(value);
    show('\n');
}

def loops() -> i32 { ... }

def main() -> null
{
    show("The final value is: ", loops());
}
*/

def pretty_show(value: i32) -> null
{
    show("The final value is: ");
    show(value);
    show('\n');
}

def loops() -> i32
{
    k: i32;

    for i = 0 to 10 {
        ++i;
    }

    loop {
        k += 2;
        if k > 10 {
            stop;
        }
    }
    k
}

def main() -> null
{
    pretty_show(loops());
}
