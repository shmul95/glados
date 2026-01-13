def show(n: i32, f: f32) -> null
{
    show("i32: ");
    show(n);
    show(", f32: ");
    show(f);
    show('\n');
}

def main() -> null
{
    n: i32 = 42;
    f: f32 = n as f32;

    show(n, f);
}
