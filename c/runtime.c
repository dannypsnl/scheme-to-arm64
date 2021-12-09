#include "representation.h"
#include <stdlib.h>

extern int scheme_entry();

#define HEAP_SIZE 0x400000

int main(int argc, const char **argv) {
  void *heap = malloc(8 * HEAP_SIZE);
  int val = scheme_entry();
  show(val);
  free(heap);
  printf("\n");
}
