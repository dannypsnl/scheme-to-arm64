#include "gc.h"
#include "representation.h"
#include <stdint.h>

int64_t _scheme_cons(int64_t car, int64_t cdr) {
  int64_t *p = (int64_t *)GC_malloc(2 * WORDSIZE);
  p[0] = car;
  p[1] = cdr;
  return ((int64_t)p) | PAIR_TAG;
}

int64_t _scheme_make_string(int64_t length, int64_t filled_by) {
  int64_t len = (int64_t)((uint64_t)length >> FIXNUM_SHIFT);
  int64_t *p = (int64_t *)GC_malloc(1 + len / 8);
  p[0] = len;
  char *p2 = (char *)(p + 1);
  for (int64_t i = 0; i <= len; i++) {
    p2[i] = filled_by >> CHAR_SHIFT;
  }
  return ((int64_t)p) | STR_TAG;
}

int64_t _scheme_make_vector(int64_t length, int64_t filled_by) {
  int64_t len = (int64_t)((uint64_t)length >> FIXNUM_SHIFT);
  int64_t *p = (int64_t *)GC_malloc(1 + len);
  p[0] = len;
  int64_t *p2 = p + 1;
  for (int64_t i = 0; i < len; i++) {
    p2[i] = filled_by;
  }
  return ((int64_t)p) | VEC_TAG;
}
