somewhere
{
    def length(s: string) -> u64;
}

/**
* public
*/

/**
* @brief compares two strings
* @param a the first string
* @param b the second string
* @return -1 if a < b, 1 if a > b, 0
*/
export def compare(a: string, b: string) -> i32
{
    len_a: u64 = length(a);
    len_b: u64 = length(b);
    min_len: u64;
    i: u64 = 0;

    if len_a < len_b {
        min_len = len_a;
    } else {
        min_len = len_b;
    }

    loop {
        if i >= min_len {
            stop;
        }

        char_a: char = a[i];
        char_b: char = b[i];

        if char_a < char_b {
            -1
        } else if char_a > char_b {
            1
        }

        ++i;
    }

    if len_a < len_b {
        -1
    } else if len_a > len_b {
        1
    }

    return 0;
}
