#include "representation.h"

void show(long x) {
  if ((x & FIXNUM_MASK) == FIXNUM_TAG) {
    // integer
    printf("%ld", x >> FIXNUM_SHIFT);
  } else if ((x & CHAR_MASK) == CHAR_TAG) {
    // character
    printf("#\\%c", (char)(x >> CHAR_SHIFT));
  } else if ((x & BOOL_MASK) == BOOL_TAG) {
    // boolean
    if ((x >> BOOL_SHIFT) != 0) {
      printf("#t");
    } else {
      printf("#f");
    }
  } else if ((x & PTR_MASK) == PAIR_TAG) {
    int *ptr = (int *)(long)(x - PAIR_TAG);
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
      ptr = (int *)(long)(cdr - PAIR_TAG);
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
  } else {
    printf("bad: %ld", x);
  }
}
