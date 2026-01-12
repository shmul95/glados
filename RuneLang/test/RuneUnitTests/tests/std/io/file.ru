somewhere
{
    def assert(condition: bool, message: string) -> bool;
    def length(s: string) -> u64;
    def compare(a: string, b: string) -> i32;

    def open_file(path: string) ~> i32;
    def close_file(fd: i32) -> bool;
    def get_file_size(fd: i32) -> i64;
    def read_file(fd: i32, size: u64) ~> string;
    def read_all(path: string) ~> string;

    def liberate(ptr: *any) -> bool;
}

/**
* public
*/

export def test_file() -> null
{
    test_open_close_valid();
    test_file_size();
    test_read_partial();
    test_read_full_content();
    test_invalid_file_handling();
}

/**
* private
*/

def test_open_close_valid() -> null
{
    path = "RuneLang/test/RuneUnitTests/tests/std/io/file.ru";
    fd = open_file(path)?;

    assert(fd >= 0, "IO: Open valid file");
    assert(close_file(fd), "IO: Close valid file descriptor");
}

def test_file_size() -> null
{
    path = "RuneLang/test/RuneUnitTests/tests/std/io/file.ru";
    fd = open_file(path)?;

    size = get_file_size(fd);
    assert(size > 0, "IO: File size should be greater than 0");

    _ = close_file(fd);
}

def test_read_partial() -> null
{
    path = "RuneLang/test/RuneUnitTests/tests/std/io/file.ru";
    fd = open_file(path)?;

    content = read_file(fd, 5)?;

    assert(length(content) <= 5, "IO: Read partial length check");

    liberate(content);
    _ = close_file(fd);
}

def test_read_full_content() -> null
{
    path = "RuneLang/test/RuneUnitTests/tests/std/io/file.ru";

    content = read_all(path)?;
    len = length(content);

    fd = open_file(path)?;
    size = get_file_size(fd);
    _ = close_file(fd);

    assert(len == size : u64, "IO: read_all length matches file size");

    is_start_correct = (content[0] == 's' && content[1] == 'o' && content[2] == 'm');
    assert(is_start_correct, "IO: read_all content start check");

    liberate(content);
}

def test_invalid_file_handling() -> null
{
    assert(!close_file(-1), "IO: close_file with invalid FD returns false");
}

def test_open_non_existent() ~> null
{
    path = "this/file/does/not/exist.txt";
    fd = open_file(path)?;

    assert(fd < 0, "IO: non-existent file fd check");
}
