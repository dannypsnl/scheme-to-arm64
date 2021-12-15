#include "gc.h"
#include "representation.h"

extern long scheme_entry();

#define HEAP_SIZE 0x400000
void *malloc(size_t size) { return GC_MALLOC(size); }

int main() {
  long val = scheme_entry();
  show(val);
  printf("\n");
}
