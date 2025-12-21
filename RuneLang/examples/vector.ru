struct Vec
{
    data:     *any;
    size:     u64;
    capacity: u64;

    /**
    * INFO: creates a new empty vector
    */
    def new() -> Vec
    {
        Vec {
            data:     null,
            size:     0,
            capacity: 0
        }
    }

    /**
    * INFO: creates a new vector from an array
    */
    override def new(value: any[]) -> Vec
    {
        vec = Vec {};

        for item in value {
            vec.push(item)?;
        }
        return vec;
    }

    /**
    * INFO: creates a new vector with an initial capacity
    */
    override def new(initial_capacity: u64) ~> Vec
    {
        vec = Vec {};

        if initial_capacity < 0 {
            error("Initial capacity cannot be negative");
        }
        vec.data = allocate(initial_capacity)?;
        vec.capacity = initial_capacity;
        return vec;
    }

    /**
    * INFO: get an element at index
    */
    def get(self, index: u64) ~> any
    {
        if index >= self.size {
            error("Index out of bounds");
        }
        return self.data[index];
    }

    /**
    * INFO: pushes a new value to the end of the vector
    */
    def push(self, value: any) ~> bool
    {
        if self.size >= self.capacity {
            new_capacity = self.capacity * 2 + 1;

            self.data = reallocate(self.data, new_capacity)?;
            self.capacity = new_capacity;
        }

        initialize(self.data, self.size, value)?;
        self.size = self.size + 1;
        return true;
    }

    /**
    * INFO: pops a value from the end of the vector
    */
    def pop(self) ~> bool
    {
        if self.size == 0 {
            error("Cannot pop from an empty vector");
        }

        self.size = self.size - 1;
        return liberate(self.data[self.size])?;
    }

    /**
    * INFO: clears the vector
    */
    def clear(self) ~> bool
    {
        for i = 0 to self.size - 1 {
            liberate(self.data[i])?;
        }
        self.size = 0;
        return true;
    }

}
