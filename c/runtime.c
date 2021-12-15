#include "gc.h"
#include "representation.h"

extern long scheme_entry();

long scm_malloc(size_t size) { return (long)GC_MALLOC(size); }

int main() {
  long val = scheme_entry();
  show(val);
  printf("\n");
}
