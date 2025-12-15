
def main() -> null
{
    octal_red: string = "\033[31mRed text(octal)\033[0m";
    hex_green: string = "\x1b[32mGreen text (hex)\x1b[0m";
    unicode_blue: string = "\u{1b}[34mBlue text (unicode)\u{1b}[0m";
    bold_red: string = "\033[1;31mBold Red text\033[0m";
    color256_red: string = "\x1b[38;5;196m256 color Red\x1b[0m";
    bg256_green: string = "\033[48;5;46m256 color Green background\033[0m";
    true_color_pink: string = "\x1b[38;2;255;105;180mRose True Color\x1b[0m";
    true_color_bg: string = "\u{1b}[48;2;0;255;255mFond Cyan True Color\u{1b}[0m";
    complex: string = "\033[1;38;2;255;255;0;48;5;19mYellow Bold on Blue Background\033[0m";

    show(octal_red);
    show('\n');
    show(hex_green);
    show('\n');
    show(unicode_blue);
    show('\n');
    show(bold_red);
    show('\n');
    show(color256_red);
    show('\n');
    show(bg256_green);
    show('\n');
    show(true_color_pink);
    show('\n');
    show(true_color_bg);
    show('\n');
    show(complex);
    show('\n');
}
