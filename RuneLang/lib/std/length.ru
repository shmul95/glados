/**
* public
*/

/**
* @brief returns the length of a string
* @param s the string
* @return the length of the string
*/
export def length(s: string) -> u64
{
    i: u64 = 0;

    lomagic: u64 = 0x0101010101010101;
    himagic: u64 = 0x8080808080808080;

    loop {

        word: u64 = s[i] : u64;
        
        if ((word - lomagic) & (~word) & himagic) != 0 {y 

            if s[i] == '\0' {
                return i;
            }
            if s[i + 1] == '\0' {
                return i + 1;
            }
            if s[i + 2] == '\0' {
                return i + 2;
            }
            if s[i + 3] == '\0' {
                return i + 3;
            }
            if s[i + 4] == '\0' {
                return i + 4;
            }
            if s[i + 5] == '\0' {
                return i + 5;
            }
            if s[i + 6] == '\0' {
                return i + 6;
            }
            if s[i + 7] == '\0' {
                return i + 7;
            }
        }

        i += 8;
    }
}

/**
* @brief returns the length of an array
* @param s the array
* @return the length of the array
*/
export override def length(s: []any) -> u64
{
    i: u64 = 0;

    for _ in s {
        ++i;
    }
    i
}
