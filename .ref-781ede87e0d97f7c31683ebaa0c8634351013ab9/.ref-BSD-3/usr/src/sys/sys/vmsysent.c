/*	vmsysent.c	2.1	1/5/80	*/

/*
 * This table is the switch used to transfer
 * to the appropriate routine for processing a vmunix special system call.
 * Each row contains the number of arguments expected
 * and a pointer to the routine.
 */

#include "../h/param.h"
#include "../h/systm.h"

int	nosys();
int	nullsys();
int	vfork();
int	vread();
int	vwrite();

struct sysent vmsysent[64] =
{
/*
 * Executing system call 64 indirect will tell whether the current
 * system is a vmunix, since this will give an error indication on
 * a non-virtual system, but just return without error on a virtual system.
 */
#define	isvmsys	nullsys
	0, 0, isvmsys,			/*  0 = isvmsys */
	0, 0, nosys,			/*  1 = nosys */
	0, 0, vfork,			/*  2 = vfork */
	3, 0, vread,			/*  3 = vread */
	3, 0, vwrite,			/*  4 = vwrite */
	0, 0, nosys,			/*  5 = nosys */
	0, 0, nosys,			/*  6 = nosys */
	0, 0, nosys,			/*  7 = nosys */
	0, 0, nosys,			/*  8 = nosys */
	0, 0, nosys,			/*  9 = nosys */
	0, 0, nosys,			/* 10 = nosys */
	0, 0, nosys,			/* 11 = nosys */
	0, 0, nosys,			/* 12 = nosys */
	0, 0, nosys,			/* 13 = nosys */
	0, 0, nosys,			/* 14 = nosys */
	0, 0, nosys,			/* 15 = nosys */
	0, 0, nosys,			/* 16 = nosys */
	0, 0, nosys,			/* 17 = nosys */
	0, 0, nosys,			/* 18 = nosys */
	0, 0, nosys,			/* 19 = nosys */
	0, 0, nosys,			/* 20 = nosys */
	0, 0, nosys,			/* 21 = nosys */
	0, 0, nosys,			/* 22 = nosys */
	0, 0, nosys,			/* 23 = nosys */
	0, 0, nosys,			/* 24 = nosys */
	0, 0, nosys,			/* 25 = nosys */
	0, 0, nosys,			/* 26 = nosys */
	0, 0, nosys,			/* 27 = nosys */
	0, 0, nosys,			/* 28 = nosys */
	0, 0, nosys,			/* 29 = nosys */
	0, 0, nosys,			/* 30 = nosys */
	0, 0, nosys,			/* 31 = nosys */
	0, 0, nosys,			/* 32 = nosys */
	0, 0, nosys,			/* 33 = nosys */
	0, 0, nosys,			/* 34 = nosys */
	0, 0, nosys,			/* 35 = nosys */
	0, 0, nosys,			/* 36 = nosys */
	0, 0, nosys,			/* 37 = nosys */
	0, 0, nosys,			/* 38 = nosys */
	0, 0, nosys,			/* 39 = nosys */
	0, 0, nosys,			/* 40 = nosys */
	0, 0, nosys,			/* 41 = nosys */
	0, 0, nosys,			/* 42 = nosys */
	0, 0, nosys,			/* 43 = nosys */
	0, 0, nosys,			/* 44 = nosys */
	0, 0, nosys,			/* 45 = nosys */
	0, 0, nosys,			/* 46 = nosys */
	0, 0, nosys,			/* 47 = nosys */
	0, 0, nosys,			/* 48 = nosys */
	0, 0, nosys,			/* 49 = nosys */
	0, 0, nosys,			/* 50 = nosys */
	0, 0, nosys,			/* 51 = nosys */
	0, 0, nosys,			/* 52 = nosys */
	0, 0, nosys,			/* 53 = nosys */
	0, 0, nosys,			/* 54 = nosys */
	0, 0, nosys,			/* 55 = nosys */
	0, 0, nosys,			/* 56 = nosys */
	0, 0, nosys,			/* 57 = nosys */
	0, 0, nosys,			/* 58 = nosys */
	0, 0, nosys,			/* 59 = nosys */
	0, 0, nosys,			/* 60 = nosys */
	0, 0, nosys,			/* 61 = nosys */
	0, 0, nosys,			/* 62 = nosys */
	0, 0, nosys,			/* 63 = nosys */
};
