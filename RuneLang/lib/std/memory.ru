somewhere
{
    def malloc(size: u64) -> *any;
    def free(ptr: *any) -> null;
    def realloc(ptr: *any, size: u64) -> *any;
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
    } else {
        free(ptr);
        true
    }
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
* private
*/
