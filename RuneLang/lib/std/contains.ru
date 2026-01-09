somewhere
{
    def length(s: string) -> u64;
}

/**
 * public
 */

/**
 * @brief checks if a string contains a substring
 * @param s the main string
 * @param sub the substring to search for
 * @return true if s contains sub, false otherwise
 */
export def contains(s: string, sub: string) -> bool
{
    len_s: u64 = length(s);
    len_sub: u64 = length(sub);

    if len_sub == 0 {
        return true;
    }
    if len_sub > len_s {
        return false;
    }

    i: u64 = 0;
    limit: u64 = len_s - len_sub;

    loop {
        if i > limit {
            stop;
        }

        if is_match_at(s, sub, i, len_sub) {
            return true;
        }
        ++i;
    }

    return false;
}

/**
 * private
 */

/**
 * @brief verifies if 'sub' matches 's' at position 'offset'
 * @param s the main string
 * @param sub the substring to search for
 * @param offset the position in 's' to check for a match
 * @param len the length of the substring
 * @return true if 'sub' matches 's' at 'offset', false otherwise
 */
def is_match_at(s: string, sub: string, offset: u64, len: u64) -> bool
{
    j: u64 = 0;

    loop {
        if j >= len {
            stop;
        }

        if s[offset + j] != sub[j] {
            return false;
        }
        ++j;
    }
    return true;
}
