somewhere
{
    def showln(message: string) -> null;
    def assert(condition: bool, message: string) -> bool;
}

abstract struct Shape
{
    protected x: f32;
    protected y: f32;

    private   id: i32;
    private static count: i32 = 0;

    public static def get_total_shapes() -> i32
    {
        Shape.count
    }

    public abstract def get_area(self) -> f32 {}

    public def move(self, dx: f32, dy: f32) -> null
    {
        self.x += dx;
        self.y += dy;
        showln("Shape moved.");
    }

    protected def new(x: f32, y: f32) -> Shape
    {
        Shape.count += 1;
        Shape {
            x: x,
            y: y,
            id: Shape.count
        }
    }
}

struct Circle extends Shape
{
    private radius: f32;

    public static def new(x: f32, y: f32, r: f32) -> Circle
    {
        base = super.new(x, y);

        Circle {
            base:   base,
            radius: r
        }
    }

    public override def get_area(self) -> f32
    {
        3.14159 * self.radius * self.radius
    }

    public override def move(self, dx: f32, dy: f32) -> null
    {
        showln("Circle is rolling...");
        super.move(dx, dy);
    }
}
