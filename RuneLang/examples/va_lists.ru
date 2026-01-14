somewhere
{
    def show(v: string) -> i32;
    def show(v: i8) -> i32;
    def show(v: i16) -> i32;
    def show(v: i32) -> i32;
    def show(v: i64) -> i32;
    def show(v: u8) -> i32;
    def show(v: u16) -> i32;
    def show(v: u32) -> i32;
    def show(v: u64) -> i32;
    def show(v: f32) -> i32;
    def show(v: f64) -> i32;
    def show(v: bool) -> i32;
    def show(v: char) -> i32;
    def show(v: *any) -> i32;
}

def show(args: ...&any) -> i32
{
    total: i32 = 0;

    for arg in args {
        total += show(arg);
    }
    total
}

def main() -> null
{
    show("Hello, World!\n", 42, '\n', 3.14: f32, '\n', true, '\n', null, '\n');
    show("Goodbye\n");
}
