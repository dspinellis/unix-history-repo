/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)profile.h	7.1 (Berkeley) %G%
 */

#define	_MCOUNT_DECL static inline void _mcount

#define	MCOUNT \
extern void mcount(cntpa) asm("mcount"); void mcount(cntpa) long **cntpa; { \
	int selfpc, frompcindex; \
	/* \
	 * Find the return address for mcount, \
	 * and address of counter pointer. \
	 */ \
	selfpc = *((char **)&cntpa-3);	/* -8(fp) */ \
	frompcindex = \
	    (unsigned short *)(*((((long *)*((char **)&cntpa-1)))-2)); \
	_mcount(frompcindex, selfpc); \
}
