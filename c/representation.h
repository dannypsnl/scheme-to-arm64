#pragma once

#include "stdio.h"

#define FIXNUM_MASK 3
#define FIXNUM_TAG 0
#define FIXNUM_SHIFT 2

#define CHAR_MASK 0xff
#define CHAR_SHIFT 8
#define CHAR_TAG 7

#define BOOL_MASK 0xff
#define BOOL_SHIFT 8
#define BOOL_TAG 15

#define PTR_MASK 7
#define PAIR_TAG 1
#define VEC_TAG 2
#define STR_TAG 3
#define VOID_TAG 5
#define CLOSURE_TAG 6

void show(long x);
