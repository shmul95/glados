/**
 * Standard Library Test (stdlib_test.ru)
 * Tests for various stdlib functions
 */

somewhere {
    use stdlib;
}

def main() -> i32
{
    show("=== RuneLang Standard Library Tests ===\n\n");

    // Test string operations
    test_string_operations();
    
    // Test memory operations  
    test_memory_operations();
    
    // Test I/O operations
    test_io_operations();
    
    // Test file operations
    test_file_operations();
    
    // Test assertions
    test_assertions();
    
    show("\n=== All tests completed ===\n");
    0
}

def test_string_operations() -> null
{
    show("--- Testing String Operations ---\n");
    
    // Test string length
    test_str: string = "Hello, World!";
    len: u64 = length(test_str);
    show("Length of '");
    show(test_str);
    show("': ");
    show(len);
    show("\n");
    
    // Test string comparison
    str1: string = "apple";
    str2: string = "banana";
    str3: string = "apple";
    
    show("Comparing '");
    show(str1);
    show("' with '");
    show(str2);
    show("': ");
    show(compare(str1, str2));
    show("\n");
    
    show("Comparing '");
    show(str1);
    show("' with '");
    show(str3);
    show("': ");
    show(compare(str1, str3));
    show("\n");
    
    // Test string contains
    haystack: string = "The quick brown fox";
    needle: string = "quick";
    missing: string = "slow";
    
    show("Does '");
    show(haystack);
    show("' contain '");
    show(needle);
    show("'? ");
    show(contains(haystack, needle));
    show("\n");
    
    show("Does '");
    show(haystack);
    show("' contain '");
    show(missing);
    show("'? ");
    show(contains(haystack, missing));
    show("\n\n");
}

def test_memory_operations() -> null
{
    show("--- Testing Memory Operations ---\n");
    
    // Test memory allocation
    buffer_size: u64 = 64;
    buffer = allocate(buffer_size);
    
    if buffer != null {
        show("Successfully allocated ");
        show(buffer_size);
        show(" bytes at address ");
        show(buffer);
        show("\n");
        
        // Test memory initialization
        init_value: u8 = 42;
        initialize(buffer, buffer_size, init_value);
        show("Initialized memory with value ");
        show(init_value);
        show("\n");
        
        // Test memory reallocation
        new_size: u64 = 128;
        buffer = reallocate(buffer, new_size);
        show("Reallocated to ");
        show(new_size);
        show(" bytes at address ");
        show(buffer);
        show("\n");
        
        // Test memory liberation
        if liberate(buffer) {
            show("Successfully freed memory\n");
        } else {
            error("Failed to free memory\n");
        }
    } else {
        error("Failed to allocate memory\n");
    }
    show("\n");
}

def test_io_operations() -> null
{
    show("--- Testing I/O Operations ---\n");
    
    // Test show with different types
    show("String: ");
    show("Hello");
    show("\n");
    
    show("Integer (i32): ");
    show(42);
    show("\n");
    
    show("Integer (i64): ");
    show(1234567890: i64);
    show("\n");
    
    show("Unsigned (u32): ");
    show(3000000000: u32);
    show("\n");
    
    show("Float (f32): ");
    show(3.14159: f32);
    show("\n");
    
    show("Float (f64): ");
    show(2.71828);
    show("\n");
    
    show("Boolean (true): ");
    show(true);
    show("\n");
    
    show("Boolean (false): ");
    show(false);
    show("\n");
    
    show("Character: ");
    show('A');
    show("\n");
    
    // Test error output
    error("This is an error message\n");
    show("\n");
}

def test_file_operations() -> null
{
    show("--- Testing File Operations ---\n");
    
    // Create a simple test file path (assuming it might exist)
    test_file: string = "README.md";
    
    show("Attempting to read file: ");
    show(test_file);
    show("\n");
    
    // Try to open and read the file
    fd = open_file(test_file);
    if fd >= 0 {
        show("File opened successfully with fd: ");
        show(fd);
        show("\n");
        
        file_size: i64 = get_file_size(fd);
        if file_size >= 0 {
            show("File size: ");
            show(file_size);
            show(" bytes\n");
            
            // Read just the first 100 bytes as a test
            read_size: u64 = 100;
            if file_size < 100 {
                read_size = file_size : u64;
            }
            
            content = read_file(fd, read_size);
            if content != null {
                show("First ");
                show(read_size);
                show(" bytes of file:\n");
                show(content);
                show("\n");
                liberate(content as *any);
            }
        }
        
        if close_file(fd) {
            show("File closed successfully\n");
        } else {
            error("Failed to close file\n");
        }
    } else {
        error("Failed to open file (this is expected if file doesn't exist)\n");
    }
    show("\n");
}

def test_assertions() -> null
{
    show("--- Testing Assertions ---\n");
    
    // Test successful assertion
    assert(true, "This assertion should pass");
    
    // Test string equality assertion
    test1: string = "hello";
    test2: string = "hello";
    assert_eq(test1, test2, "String equality test should pass");
    
    // Test failed assertion (this will show as failed)
    assert(false, "This assertion should fail");
    
    // Test failed string equality 
    test3: string = "world";
    assert_eq(test1, test3, "String inequality test should fail");
    
    show("\n");
}
