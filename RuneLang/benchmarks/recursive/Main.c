#include <stdio.h>

long fib(long n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

int main(void) {
    printf("result: %ld\n", fib(40));
    return 0;
}

