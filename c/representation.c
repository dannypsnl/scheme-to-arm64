#include "representation.h"

void show(int x) {
  if ((x & FIXNUM_MASK) == FIXNUM_TAG) {
    // integer
    printf("%d", x >> FIXNUM_SHIFT);
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
    int *ptr = (int *)(size_t)(x - PAIR_TAG);
    if (ptr == NULL) {
      printf("()");
      return;
    }

    // either a list or a dotted pair
    int car = ptr[0];
    int cdr = ptr[1];
    putchar('(');
    show(car);

    // show additional space-separated elems
    while ((cdr & PTR_MASK) == PAIR_TAG) {
      ptr = (int *)(size_t)(cdr - PAIR_TAG);
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
    printf("bad:%d", x);
  }
}
