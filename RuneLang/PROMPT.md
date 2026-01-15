add "extern" keyword for the somewhere glibc import.
functions with "extern" keyword will **not** be mangled.

otherwise, all somewhere { } function **must** be mangled.

the goal is to make && make lib && rune build examples/va_lists.ru

also, do **NOT** remove in the optimizer functions with "export" keyword, no inline.
