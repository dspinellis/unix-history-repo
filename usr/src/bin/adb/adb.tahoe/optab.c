#ifndef lint
static char sccsid[] = "@(#)optab.c	1.2 (Berkeley) 1/16/89";
#endif

#include "defs.h"
#include "optab.h"

struct optab optab[] = {
#define OP(a,b,c,d,e,f,g,h,i) {a,b,c,d,e,f,g,h,i}
#include "instrs.adb"
	0
};

char *regname[] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10", "r11", "r12", "fp", "sp", "pc"
};
