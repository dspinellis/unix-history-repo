/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)profile.h	7.2 (Berkeley) %G%
 */

#define	_MCOUNT_DECL static inline void _mcount

#define	MCOUNT \
extern void mcount() asm("mcount"); void mcount() { \
	int selfpc, frompcindex; \
	/* \
	 * find the return address for mcount, \
	 * and the return address for mcount's caller. \
	 * \
	 * selfpc = pc pushed by mcount call \
	 */ \
	asm("movl 4(%%ebp),%0" : "=r" (selfpc)); \
	/* \
	 * frompcindex = pc pushed by jsr into self. \
	 * In GCC the caller's stack frame has already been built so we \
	 * have to chase a6 to find caller's raddr. \
	 */ \
	asm("movl (%%ebp),%0" : "=r" (frompcindex)); \
	frompcindex = ((int *)frompcindex)[1]; \
	_mcount(frompcindex, selfpc); \
}
