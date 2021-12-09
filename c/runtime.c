#include "representation.h"
#include <stdlib.h>

extern int scheme_entry();

#define HEAP_SIZE 0x400000

int main(int argc, const char **argv) {
  int *heap = malloc(8 * HEAP_SIZE);
  printf("heap: %p\n", heap);
  int val = scheme_entry(heap);
  show(val);
  free(heap);
  puts("\n");
}
