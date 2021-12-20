#include "gc.h"
#include "representation.h"

extern long scheme_entry();

long _scheme_cons(long car, long cdr) {
  long *p = (long *)GC_malloc(2 * WORDSIZE);
  p[0] = car;
  p[1] = cdr;
  return ((long)p) | PAIR_TAG;
}

long _scheme_make_string(long length, long filled_by) {
  long len = (long)((unsigned long)length >> FIXNUM_SHIFT);
  long *p = (long *)GC_malloc(len);
  p[0] = len;
  char *p2 = (char *)p[1];
  for (long i = 1; i < len + 1; i++) {
    p2[i] = (char)filled_by;
  }
  return ((long)p) | STR_TAG;
}

long _scheme_make_vector(long length, long filled_by) {
  long len = (long)((unsigned long)length >> FIXNUM_SHIFT);
  long *p = (long *)GC_malloc(len);
  p[0] = len;
  for (long i = 1; i < len + 1; i++) {
    p[i] = filled_by;
  }
  return ((long)p) | VEC_TAG;
}

int main() {
  long val = scheme_entry();
  show(val);
  printf("\n");
}
