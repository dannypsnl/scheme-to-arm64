#include "representation.h"

extern long scheme_entry();

int main() {
  long val = scheme_entry();
  show(val);
  printf("\n");
}
