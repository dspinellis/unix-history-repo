#include "crt0.h"

static long __dtor_prev = 0, __dtor_next = 0;

#ifdef sparc
asm (".stabs \"___CTOR_LIST__\", 0x14, 0, 0, 0\n");
asm (".stabs \"___CTOR_LIST__\", 0x14, 0, 0, 0\n");
asm (".stabs \"___DTOR_LIST__\", 0x18, 0, 0, ___dtor_next\n");
asm (".stabs \"___DTOR_LIST__\", 0x18, 0, 0, ___dtor_prev\n");
asm (".stabs \"___ZTOR_LIST__\", 0x14, 0, 0, 0\n");
asm (".stabs \"___ZTOR_LIST__\", 0x14, 0, 0, 0\n");
#endif
