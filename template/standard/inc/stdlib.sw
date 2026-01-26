/**
 * RuneLang Standard Library (stdlib.sw)
 * Function signatures from lib/std
 */

// ========== MEMORY MANAGEMENT ==========
def allocate(size: u64) ~> *any;
def liberate(ptr: *any) -> bool;
def reallocate(ptr: *any, size: u64) ~> *any;
def initialize(ptr: *any, size: u64, value: u8) ~> *any;

// ========== STRING OPERATIONS ==========
def length(s: string) -> u64;
def compare(a: string, b: string) -> i32;
def contains(s: string, sub: string) -> bool;

// ========== I/O OPERATIONS - SHOW ==========
def show(v: string) -> i32;
def show(v: i8) -> i32;
def show(v: i16) -> i32;
def show(v: i32) -> i32;
def show(v: i64) -> i32;
def show(v: u8) -> i32;
def show(v: u16) -> i32;
def show(v: u32) -> i32;
def show(v: u64) -> i32;
def show(v: f32) -> i32;
def show(v: f64) -> i32;
def show(v: bool) -> i32;
def show(v: char) -> i32;
def show(v: *any) -> i32;

// ========== I/O OPERATIONS - ERROR ==========
def error(v: string) -> null;
def error(v: i8) -> null;
def error(v: i16) -> null;
def error(v: i32) -> null;
def error(v: i64) -> null;
def error(v: u8) -> null;
def error(v: u16) -> null;
def error(v: u32) -> null;
def error(v: u64) -> null;
def error(v: f32) -> null;
def error(v: f64) -> null;
def error(v: bool) -> null;
def error(v: char) -> null;
def error(v: *any) -> null;

// ========== FILE OPERATIONS ==========
def open_file(path: string) ~> i32;
def close_file(fd: i32) -> bool;
def get_file_size(fd: i32) -> i64;
def read_file(fd: i32, size: u64) ~> string;
def read_all(path: string) ~> string;

// ========== ASSERTION FUNCTIONS ==========
def assert(condition: bool, message: string) -> bool;
def assert_eq(str1: string, str2: string, message: string) -> bool;