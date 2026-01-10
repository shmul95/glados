somewhere
{
    def assert(condition: bool, message: string) -> bool;

    def allocate(size: u64) ~> *any;
    def liberate(ptr: *any) -> bool;
    def reallocate(ptr: *any, size: u64) ~> *any;
}

/**
* public
*/

export def test_memory() -> null
{
    test_allocate();
    test_liberate();
    test_reallocate();
    test_null_handling();
    test_multiple_allocations();
}

/**
* private
*/

def test_allocate() -> null
{
    ptr: *any = allocate(64)?;
    assert(ptr != null, "Allocate: basic allocation returns non-null");
    assert(liberate(ptr), "Allocate: basic allocation freed successfully");

    ptr2: *any = allocate(1)?;
    assert(ptr2 != null, "Allocate: minimum allocation (1 byte)");
    assert(liberate(ptr2), "Allocate: minimum allocation freed successfully");

    ptr3: *any = allocate(4096)?;
    assert(ptr3 != null, "Allocate: page-size allocation (4096 bytes)");
    assert(liberate(ptr3), "Allocate: page-size allocation freed successfully");

    ptr4: *any = allocate(1024 * 1024)?;
    assert(ptr4 != null, "Allocate: large allocation (1MB)");
    assert(liberate(ptr4), "Allocate: large allocation freed successfully");
}

def test_liberate() -> null
{
    ptr: *any = allocate(128)?;
    assert(ptr != null, "Liberate: allocation before free");
    assert(liberate(ptr), "Liberate: free returns true for valid pointer");
}

def test_reallocate() -> null
{
    ptr: *any = allocate(64)?;
    assert(ptr != null, "Reallocate: initial allocation");

    ptr2: *any = reallocate(ptr, 128)?;
    assert(ptr2 != null, "Reallocate: grow allocation (64 -> 128)");

    ptr3: *any = reallocate(ptr2, 32)?;
    assert(ptr3 != null, "Reallocate: shrink allocation (128 -> 32)");

    ptr4: *any = reallocate(ptr3, 4096)?;
    assert(ptr4 != null, "Reallocate: large growth (32 -> 4096)");

    assert(liberate(ptr4), "Reallocate: final free after reallocations");
}

def test_null_handling() -> null
{
    assert(!liberate(null), "Null handling: liberate(null) returns false");
}

def test_multiple_allocations() -> null
{
    ptr1: *any = allocate(32)?;
    ptr2: *any = allocate(64)?;
    ptr3: *any = allocate(128)?;
    ptr4: *any = allocate(256)?;

    assert(ptr1 != null, "Multiple: allocation 1");
    assert(ptr2 != null, "Multiple: allocation 2");
    assert(ptr3 != null, "Multiple: allocation 3");
    assert(ptr4 != null, "Multiple: allocation 4");

    assert(ptr1 != ptr2, "Multiple: ptr1 != ptr2");
    assert(ptr2 != ptr3, "Multiple: ptr2 != ptr3");
    assert(ptr3 != ptr4, "Multiple: ptr3 != ptr4");

    assert(liberate(ptr1), "Multiple: ptr1 freed successfully");
    assert(liberate(ptr2), "Multiple: ptr2 freed successfully");
    assert(liberate(ptr3), "Multiple: ptr3 freed successfully");
    assert(liberate(ptr4), "Multiple: ptr4 freed successfully");
}
