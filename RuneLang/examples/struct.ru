somewhere
{
    def show(v: string) -> i32;
    def show(v: f32) -> i32;
    def show(v: i32) -> i32;
}

abstract struct Vec2f
{
    private x: f32 = 0.0;
    private y: f32 = 1.0;

    public static count: i32 = 20;

    public def new() -> Vec2f
    {
        Vec2f.count += 1;
        Vec2f {}
    }

    public def new(x: f32, y: f32) -> Vec2f
    {
        Vec2f.count += 1;
        Vec2f { x: x, y: y }
    }

    public def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.get_x(),
            y: self.y + other.get_y()
        }
    }

    public def add(self, f: f32) -> Vec2f
    {
        Vec2f {
            x: self.x + f,
            y: self.y + f
        }
    }

    public def get_x(self) -> f32
    {
        self.x
    }

    public def get_y(self) -> f32
    {
        self.y
    }

    private def print_hello(self) -> null
    {
        show("Hello from Vec2f!\n");
    }
}

def show(prefix: string, v: Vec2f) -> null
{
    show(prefix);
    show("(x: ");
    show(v.get_x());
    show(", y: ");
    show(v.get_y());
    show(")\n");
}

def main() -> null
{
}
