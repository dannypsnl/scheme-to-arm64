#include "representation.h"

void show(long x) {
  if ((x & FIXNUM_MASK) == FIXNUM_TAG) {
    // integer
    printf("%ld", x >> FIXNUM_SHIFT);
  } else if ((x & CHAR_MASK) == CHAR_TAG) {
    // character
    char c = (char)(x >> CHAR_SHIFT);
    switch (c) {
    case '\0':
      printf("#\\nul");
      break;
    case ' ':
      printf("#\\space");
      break;
    case '\n':
      printf("#\\newline");
      break;
    default:
      printf("#\\%c", c);
    }
  } else if ((x & BOOL_MASK) == BOOL_TAG) {
    // boolean
    if ((x >> BOOL_SHIFT) != 0) {
      printf("#t");
    } else {
      printf("#f");
    }
  } else if ((x & PTR_MASK) == VOID_TAG) {
  } else if ((x & PTR_MASK) == PAIR_TAG) {
    long *ptr = (long *)(x - PAIR_TAG);
    if (ptr == NULL) {
      printf("()");
      return;
    }
    // either a list or a dotted pair
    long car = ptr[0];
    long cdr = ptr[1];
    putchar('(');
    show(car);
    // show additional space-separated elems
    while ((cdr & PTR_MASK) == PAIR_TAG) {
      ptr = (long *)(cdr - PAIR_TAG);
      if (ptr == NULL)
        break;

      car = ptr[0];
      cdr = ptr[1];
      putchar(' ');
      show(car);
    }
    // show dotted pair notation if relevant
    if ((cdr & PTR_MASK) != PAIR_TAG) {
      printf(" . ");
      show(cdr);
    }
    putchar(')');
  } else if ((x & PTR_MASK) == STR_TAG) {
    long *ptr = (long *)(x - STR_TAG);
    long len = *ptr;
    char *body = (char *)(ptr + 1);
    putchar('"');
    for (; len > 0; len--)
      putchar(*body++);
    putchar('"');
  } else if ((x & PTR_MASK) == VEC_TAG) {
    long *ptr = (long *)(x - VEC_TAG);
    long len = *ptr++;
    printf("#(");
    for (; len > 0; len--) {
      show(*ptr++);
      if (len != 1)
        putchar(' ');
    }
    printf(")");
  } else {
    printf("bad: %ld", x);
  }
}
