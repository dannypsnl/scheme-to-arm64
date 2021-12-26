#include "gc.h"
#include "representation.h"

extern int64_t scheme_entry();

int64_t _scheme_cons(int64_t car, int64_t cdr) {
  int64_t *p = (int64_t *)GC_malloc(2 * WORDSIZE);
  p[0] = car;
  p[1] = cdr;
  return ((int64_t)p) | PAIR_TAG;
}

int main() {
  int64_t val = scheme_entry();
  show(val);
  printf("\n");
}
