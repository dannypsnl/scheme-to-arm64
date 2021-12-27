#include "gc.h"
#include <stdint.h>

#define WORDSIZE 8
#define PAIR_TAG 1

int64_t _scheme_cons(int64_t car, int64_t cdr) {
  int64_t *p = (int64_t *)GC_malloc(2 * WORDSIZE);
  p[0] = car;
  p[1] = cdr;
  return ((int64_t)p) | PAIR_TAG;
}