somewhere
{
    def show(v: string) -> i32;
    def show(v: f32) -> i32;
}

struct Vec2f
{
    x: f32;
    y: f32;

    def new() -> Vec2f
    {
        Vec2f { x: 0.0, y: 0.0 }
    }

    def new(x: f32, y: f32) -> Vec2f
    {
        Vec2f { x: x, y: y }
    }

    def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }

    def add(self, f: f32) -> Vec2f
    {
        Vec2f {
            x: self.x + f,
            y: self.y + f
        }
    }
}

def show(v: Vec2f) -> null
{
    show("Vec2f(x: ");
    show(v.x);
    show(", y: ");
    show(v.y);
    show(")\n");
}

def main() -> null
{
    a = Vec2f { x: 1.0, y: 2.0 };
    b = Vec2f.new(3.0, 4.0);

    c = a.add(b);
    d = a.add(5.0);

    show(c);
}
