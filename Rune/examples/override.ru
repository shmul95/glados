/*
as it is an override of show(value: any) it will end up in the IR as the label:
show_i32_f32:
*/
override def show(value: i32, other: f32) -> null
{
    show("i32 and f32: ");
    show(value);
    show(", ");
    show(other);
    show("\n");
}

/*
as it is an override of add(i32, i32) it will end up in the IR as the label:
add_f32_f32:
*/
override def add(a: f32, b: f32) -> f32
{
    a + b + 1.0
}

/*
works like:
add:
*/
def add(a: i32, b: i32) -> i32
{
    a + b
}

def main() -> null
{
    i32_result = add(-42, 69);
    f32_result = add(-64.6, 32.4);

    show(i32_result, f32_result);
}
