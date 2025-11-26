def main() -> null
{
    str: string = "Warszawa";
    num: i32 = 1;
    k: i32;

    for c in str {
        if c == 'a' {
            next;
        }
        show(c);
    }

    for i = 0 to 10 {
        ++i;
    }

    for j: i32 = 0 to 25 {
        j *= 2;
    }
    k = j;

    loop 
    {
        if num > k {
            stop; 
        }

        num += 3;
    }

    show(i);
    show(j);
    show(num);
}
