#include "representation.h"
#include <stdlib.h>

extern long scheme_entry(long *);

#define HEAP_SIZE 0x400000

int main(int argc, const char **argv) {
  long *heap = malloc(8 * HEAP_SIZE);
  long val = scheme_entry(heap);
  show(val);
  free(heap);
  printf("\n");
}
