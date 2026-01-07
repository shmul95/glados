#include <stdio.h>

int main(void) {
  long result = 0;

  for (long i = 0; i < 1000000000; ++i) {
    result += i;
  }
  printf("%ld\n", result);
}
