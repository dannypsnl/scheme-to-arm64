#include "gc.h"
#include "representation.h"

extern long scheme_entry();

long _scheme_cons(long car, long cdr) {
  long *p = (long *)GC_malloc(2 * WORDSIZE);
  p[0] = car;
  p[1] = cdr;
  return ((long)p) | 1;
}

int main() {
  long val = scheme_entry();
  show(val);
  printf("\n");
}
