somewhere
{
    def allocate(size: u64) ~> *any;
    def liberate(ptr: *any) -> bool;
    def reallocate(ptr: *any, size: u64) ~> *any;
    def initialize(ptr: *any, size: u64, value: u8) ~> *any;
    
    def assert(condition: bool, message: string) -> bool;
    def assert_eq(a: string, b: string, message: string) -> bool;
}

struct Vec
{
    data:     *any;
    size:     u64;
    capacity: u64;

    def new() -> Vec
    {
        Vec {
            data:     null,
            size:     0,
            capacity: 0
        }
    }

    override def new(initial_capacity: u64) ~> Vec
    {
        if initial_capacity == 0 {
            new()
        }

        ptr = allocate(initial_capacity * 8)?; 

        Vec {
            data:     ptr,
            size:     0,
            capacity: initial_capacity
        }
    }

    def get(self, index: u64) ~> any
    {
        if index >= self.size {
            error("Index out of bounds")
        }
        self.data[index]
    }

    def reserve(self, new_capacity: u64) ~> bool
    {
        if new_capacity > self.capacity {
            self.data = reallocate(self.data, new_capacity * 8)?;
            self.capacity = new_capacity;
        }
        true
    }

    def push(self, value: any) ~> bool
    {
        if self.size >= self.capacity {
            new_capacity = self.capacity * 2 + 1;
            self.data = reallocate(self.data, new_capacity * 8)?;
            self.capacity = new_capacity;
        }

        self.data[self.size] = value;
        self.size = self.size + 1;
        true
    }

    def pop(self) ~> any
    {
        if self.size == 0 {
            error("Cannot pop from an empty vector")
        }
        self.size = self.size - 1;
        self.data[self.size]
    }

    def clear(self) -> bool
    {
        if self.data != null {
            liberate(self.data);
        }
        self.data = null;
        self.size = 0;
        self.capacity = 0;
        true
    }

    def delete(self) -> bool
    {
        self.clear()
    }

}

def vec_number() -> null
{
    v = Vec.new(4)?;

    v.push(10)?;
    v.push(20)?;
    v.push(30)?;

    assert(v.get(1)? == 20, "Le deuxième élément doit être 20");

    v.clear();
    assert(v.size == 0, "La taille doit être 0 après clear()");

    v.reserve(2)?;
    v.push(40)?;
    v.push(50)?;

    assert(v.pop()? == 50, "Le dernier élément doit être 50");

    v.delete(); // normalement, delete() doit être appelé à la fin du scope de v. comme le C++.
}

def vec_string() -> null
{
    v = Vec {}; // équivalent de v = Vec.new();

    v.reserve(3)?;

    v.push("Bonjour")?; // no allocation needed
    v.push("le")?;      // no allocation needed
    v.push("monde")?;   // no allocation needed

    assert_eq(v.get(0)?, "Bonjour", "Le premier élément doit être 'Bonjour'");
    assert_eq(v.pop()?, "monde", "Le dernier élément doit être 'monde'");

    v.delete(); // normalement, delete() doit être appelé à la fin du scope de v. comme le C++.
}

def main() ~> null
{
    vec_number();
    vec_string();
}
