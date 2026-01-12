struct Vec2f
{
    public x: f32;
    private y: f32;

    protected def new() -> Vec2f
    {
        show("Default constructor called\n");
        Vec2f { x: 0.0, y: 0.0 }
    }

    override def new(x: f32, y: f32) -> Vec2f
    {
        show("Parameterized constructor called\n");
        Vec2f { x: x, y: y }
    }

    def add(self, other: Vec2f) -> Vec2f
    {
        show("Adding two Vec2f instances\n");
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }

    override def add(self, f: f32) -> Vec2f
    {
        show("Adding scalar to Vec2f instance\n");
        Vec2f {
            x: self.x + f,
            y: self.y + f
        }
    }
}

override def show(prefix: string, v: Vec2f) -> null
{
    show(prefix);
    show("(x: ");
    show(v.x);
    show(", y: ");
    show(v.y);
    show(")\n");
}

def main() -> null
{
    show("--------------------\nCreating Vec2f instances:\n\n");
    v1 = Vec2f.new();
    show("v1: ", v1);
    v2 = Vec2f.new(3.0, 4.0);
    show("v2: ", v2);
    v3 = Vec2f { x: 1.0, y: 2.0 };
    show("v3: ", v3);
    show("--------------------\n");
}
