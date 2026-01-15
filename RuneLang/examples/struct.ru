struct Vec2f
{
    private x: f32 = 0.0;
    private y: f32 = 0.0;

    public static count: i32 = 0;

    public def new() -> Vec2f
    {
        show("Default constructor called\n");
        Vec2f.count += 1;
        Vec2f { x: 0.0, y: 0.0 }
    }

    public def new(x: f32, y: f32) -> Vec2f
    {
        show("Parameterized constructor called\n");
        Vec2f.count += 1;
        Vec2f { x: x, y: y }
    }

    public def add(self, other: Vec2f) -> Vec2f
    {
        show("Adding two Vec2f instances\n");
        Vec2f {
            x: self.x + other.get_x(),
            y: self.y + other.get_y()
        }
    }

    public def add(self, f: f32) -> Vec2f
    {
        show("Adding scalar to Vec2f instance\n");
        Vec2f {
            x: self.x + f,
            y: self.y + f
        }
    }

    public def get_x(self) -> f32
    {
        self.print_hello();
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
    v1 = Vec2f.new();
    v2 = Vec2f.new(3.0, 4.0);

    show(Vec2f.count);
    show("v1: ", v1);
    show("v2: ", v2);
}
