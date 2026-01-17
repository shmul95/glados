somewhere
{
    extern def open(path: string, flags: i32) -> i32;
    extern def read(fd: i32, buf: *any, count: u64) -> i64;
    extern def close(fd: i32) -> i32;
    extern def lseek(fd: i32, offset: i64, whence: i32) -> i64;

    def allocate(size: u64) ~> *any;
    def liberate(ptr: *any) -> bool;

    def error(v: string) -> null;
    def error(v: char) -> null;
}

/**
* public
*/

/**
 * @brief opens a file in read-only mode safely
 * @param path the path to the file
 * @return the file descriptor
 */
export def open_file(path: string) ~> i32
{
    o_rdonly: i32 = 0;
    fd = open(path, o_rdonly);

    if fd < 0 {
        error("io: failed to open file: ");
        error(path);
        error("\n")
    }
    fd
}

/**
 * @brief closes an open file descriptor
 * @param fd the file descriptor to close
 * @return true if successfully closed, false otherwise
 */
export def close_file(fd: i32) -> bool
{
    if fd < 0 {
        false
    }
    close(fd) == 0
}

/**
 * @brief retrieves the total size of a file in bytes
 * @param fd the file descriptor
 * @return the size of the file in bytes, or -1 on error
 */
export def get_file_size(fd: i32) -> i64
{
    seek_set: i32 = 0;
    seek_end: i32 = 2;

    size = lseek(fd, 0: i64, seek_end);
    lseek(fd, 0: i64, seek_set);

    size
}

/**
 * @brief reads the content of an already opened file
 * @param fd the file descriptor
 * @param size the number of bytes to read
 * @return the content as a null-terminated string
 */
export def read_file(fd: i32, size: u64) ~> string
{
    buffer = allocate(size + 1)?;

    bytes_read = read(fd, buffer, size);

    if bytes_read < 0 {
        liberate(buffer);
        error("io: error while reading file content");
        error("\n")
    }

    buffer[bytes_read] = '\0';
    buffer as string
}

/**
 * @brief safely reads an entire file from a path
 * @param path the path to the file
 * @return the file content as a null-terminated string
 */
export def read_all(path: string) ~> string
{
    fd = open_file(path)?;

    file_size = get_file_size(fd);
    if file_size < 0 {
        close_file(fd);
        error("io: failed to get file size: ");
        error(path);
        error("\n")
    }

    content = read_file(fd, file_size : u64)?;

    close_file(fd);
    content
}
