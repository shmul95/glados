def main() -> null
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
    show(k);
}
