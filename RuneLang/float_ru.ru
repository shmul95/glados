def show_float(value: f32) -> null
{
    show(value);
    show('\n');
}

def main() -> i32
{
    a: f32 = 42.0;

    show_float(a);
    show_float(23.0);
    0
}
