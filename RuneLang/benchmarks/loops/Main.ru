def main() -> null
{
    total_steps: i64 = 0;

    for i: i64 = 1 to 1_000_001 {
        n: i64 = i;
        
        loop {
            if n == 1 {
                stop;
            }
            if n % 2 == 0 {
                n /= 2;
            } else {
                n = n * 3 + 1;
            }
            total_steps = total_steps + 1;
        }
        ++i;
    }
    show(total_steps);
}
