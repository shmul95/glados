somewhere
{
    def show(msg: string) -> i32;
    def show(v: f32) -> i32;
}

abstract struct Shape
{
    protected x: f32 = 0.0;
    protected y: f32 = 0.0;

    public def new(x: f32, y: f32) -> Shape
    {
        Shape { x: x, y: y }
    }

    public def move(self) -> null
    {
        show("Moving shape to new position...\n");
        self.x += 1.0;
        self.y += 1.0;
    }

    protected def print_hola(self) -> null
    {}

    public static def say_hi() -> null
    {
    }
}

struct Circle extends Shape
{
    public radius: f32 = 1.0;
    private v: f32 = 42.0;

    public def new(x: f32, y: f32, radius: f32) -> Circle
    {
        Circle {
            __base: Shape.new(x, y),
            radius: radius
        }
    }

    public def say_hola(self) -> null
    {
        self.x;
        self.print_hola();
    }
}

def show(msg: string, c: Circle) -> null
{
    show(msg);
    show("(x: ");
    //show(c.x);
    show(", y: ");
    //show(c.y);
    show(", radius: ");
    //show(c.radius);
    show(")\n");
    //c.move();
}

def main() -> null
{
    c = Circle.new(5.0, 10.0, 15.0);
    c.say_hola();
    show("Circle details: ", c);
}