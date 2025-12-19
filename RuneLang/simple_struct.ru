struct Vec2f
{
    x: i32;
    y: i32;

    def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        };
    }

    def add(self, scale: i32) -> Vec2f
    {
        Vec2f {
            x: self.x * scale,
            y: self.y * scale
        };
    }
}

override def show(a: i32, b: i32) -> null
{
    show("Vec2f(");
    show(a);
    show(", ");
    show(b);
    show(")\n");
}

def main() -> null
{
    a: Vec2f = Vec2f { x: 10, y: 20 };
    b: Vec2f = Vec2f { x: 5, y: 7 };
    v: i32 = 42;

    c: Vec2f = a.add(b);
    d: Vec2f = c.add(v);

    show(12, 24);
}
