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
extern void mcount() asm("mcount"); void mcount() { \
	int selfpc, frompcindex; \
	asm("movl a6@(4),%0" : "=r" (selfpc)); \
	asm("movl a6@(0)@(4),%0" : "=r" (frompcindex)); \
	_mcount(frompcindex, selfpc); \
}

#ifdef KERNEL
/*
 * The following two macros do splhigh and splx respectively.
 * They have to be defined this way because these are real
 * functions on the HP, and we do not want to invoke mcount
 * recursively.
 */
#define MCOUNT_ENTER \
	asm("movw	sr,%0" : "=g" (s)); \
	asm("movw	#0x2700,sr")

#define MCOUNT_EXIT \
	asm("movw	%0,sr" : : "g" (s))
#endif /* KERNEL */
