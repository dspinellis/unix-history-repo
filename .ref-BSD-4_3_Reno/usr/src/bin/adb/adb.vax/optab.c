#ifndef lint
static char optab_sccsid[] = "@(#)optab.c	4.5 (Berkeley) 1/16/89";
#endif

/*
 * adb - (read-only) tables for VAX instruction decoding
 */

#define	ADB
#undef INSTTAB

/*
 * Pick up definitions for insttab from the assembler, and also
 * the arrays ty_NORELOC and ty_nbyte (and soon ty_float);
 * then, make insttab.
 */
#include <sys/types.h>
#include "instrs.h"
#include "assizetab.c"

struct insttab insttab[] = {
#include "instrs.adb"
	0
};

/*
 * Register names, and floating point immediate constants.
 * The f.p. constants can be derived from the expression
 *
 *	fp = (.5 + (n & 7) / 16.0) x (2 ^ (n >> 3))
 *
 * or, alternatively,
 *
 *	union { int i; float f; } u; u.i = (n << 3) | (1 << 14); u.f
 *
 * but there are only 64 of them, and this is simpler.
 */
char *regname[16] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10","r11","ap", "fp", "sp", "pc"
};
char *fltimm[64] = {
	"0.5", "0.5625", "0.625", "0.6875", "0.75", "0.8125", "0.875", "0.9375",
	"1.0", "1.125", "1.25", "1.375", "1.5", "1.625", "1.75", "1.875",
	"2.0", "2.25", "2.5", "2.75", "3.0", "3.25", "3.5", "3.75",
	"4.0", "4.5", "5.0", "5.5", "6.0", "6.5", "7.0", "7.5",
	"8.0", "9.0", "10.0", "11.0", "12.0", "13.0", "14.0", "15.0",
	"16.0", "18.0", "20.0", "22.0", "24.0", "26.0", "28.0", "30.0",
	"32.0", "36.0", "40.0", "44.0", "48.0", "52.0", "56.0", "60.0",
	"64.0", "72.0", "80.0", "88.0", "96.0", "104.0", "112.0", "120.0"
};
