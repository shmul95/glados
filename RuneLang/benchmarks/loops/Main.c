#include <stdio.h>

int main(void) {

  long long total_steps = 0;

  for (int i = 1; i <= 1000000; ++i) {
    long long n = i;

    while (n != 1) {

      if (n % 2 == 0) {
        n = n / 2;
      } else {
        n = 3 * n + 1;
      }
      ++total_steps;
    }
  }

  printf("%lld", total_steps);
  return 0;
}
