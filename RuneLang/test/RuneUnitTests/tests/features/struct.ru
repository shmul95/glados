somewhere
{
    def allocate(size: u64) ~> *any;
    def liberate(ptr: *any) -> bool;
    def reallocate(ptr: *any, size: u64) ~> *any;
    def initialize(ptr: *any, size: u64, value: u8) ~> *any;
    
    def assert(condition: bool, message: string) -> bool;
    def assert_eq(a: string, b: string, message: string) -> bool;

    def error(s: string) -> null;
}

/**
* public
*/

export def test_struct() -> null
{
    vec_number();
    vec_string();
}

/**
* private
*/

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

    def new(initial_capacity: u64) ~> Vec
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
            error("Vec::get: index out of bounds\n")
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
            error("Vec::pop: cannot pop from an empty vector\n")
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

    assert(v.get(1)? == 20, "Struct: second element must be 20");

    v.clear();
    assert(v.size == 0, "Struct: size must be 0 after clear()");

    v.reserve(2)?;
    v.push(40)?;
    v.push(50)?;

    assert(v.pop()? == 50, "Struct: last element must be 50");

    v.delete()
}

def vec_string() -> null
{
    v = Vec {};

    v.reserve(3)?;

    v.push("Bonjour")?;
    v.push("le")?;
    v.push("monde")?;

    assert_eq(v.get(0)?, "Bonjour", "Struct: first element must be 'Bonjour'");
    assert_eq(v.pop()?, "monde", "Struct: last element must be 'monde'");

    v.delete();
}
