def main() -> null
{
    str: string = "Warszawa";
    num: i32 = 1;

    for c in str {
        show(c);
    }

    for i = 0 to 10 {
        ++i;
    }

    for j: i32 = 0 to 25 {
        j *= 2;
    }

    loop 
    {
        if num > j {
            stop; 
        }

        num += 3;
    }

    show(i);
    show(j);
    show(num);
}
