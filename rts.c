#include <stdio.h>
#include <stdlib.h>

extern int scheme_entry();

int main(int argc, const char **argv) {
  int val = scheme_entry();
  printf("%d\n", val);
  return 0;
}
