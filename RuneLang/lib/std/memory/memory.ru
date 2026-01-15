somewhere
{
    extern def malloc(size: u64) -> *any;
    extern def free(ptr: *any) -> null;
    extern def realloc(ptr: *any, size: u64) -> *any;
    extern def memset(ptr: *any, value: u8, size: u64) -> *any;

    def error(v: string) -> i32;
}

/**
* public
*/

/**
* @brief allocate memory of given size
* @param size size in bytes
* @return pointer to allocated memory
*/
export def allocate(size: u64) ~> *any
{
    ptr = malloc(size);

    if ptr == null {
        error("allocate: memory allocation failed.\n")
    }
    ptr
}

/**
* @brief liberate memory at given pointer
* @param ptr pointer to memory to deallocate
* @return true if freed, false if null
*/
export def liberate(ptr: *any) -> bool
{
    if ptr == null {
        false
    }
    free(ptr);
    true
}

/**
* @brief reallocate memory at given pointer to new size
* @param ptr pointer to memory to reallocate
* @param size new size in bytes
* @return pointer to reallocated memory
*/
export def reallocate(ptr: *any, size: u64) ~> *any
{
    new_ptr = realloc(ptr, size);

    if new_ptr == null {
        error("reallocate: memory reallocation failed.\n")
    }

    new_ptr
}

/**
* @brief initialize memory at given pointer with a value
* @param ptr pointer to memory
* @param size size in bytes
* @param value byte value to set
* @return pointer to initialized memory
*/
export def initialize(ptr: *any, size: u64, value: u8) ~> *any
{
    if ptr == null {
        error("initialize: pointer is null.\n")
    }

    if size > 0 {
        memset(ptr, value, size)
    }
    ptr
}

/**
* private
*/
