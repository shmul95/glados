override def show(msg: string, value: i32) -> null
{
    show(msg);
    show(value);
    show('\n');
}

def loops() -> i32
{
    k: i32 = 0;

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
    show("The final value is: ", loops());
}

