def main() -> null
{
    show("\033[31mRouge (octal)\033[0m");
    show('\n');
    show("\x1b[32mVert (hex)\x1b[0m");
    show('\n');
    show("\u{1b}[34mBleu (unicode)\u{1b}[0m");
    show('\n');
    show("\033[1;33mJaune Gras\033[0m");
    show('\n');
}
