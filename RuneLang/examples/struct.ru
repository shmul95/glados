struct Vec2f
{
    x: f32;
    y: f32;

    def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }

}

override def show(v: Vec2f) -> null
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
    b = Vec2f { x: 3.0, y: 4.0 };
    c = a.add(b);

    show(c);
}
