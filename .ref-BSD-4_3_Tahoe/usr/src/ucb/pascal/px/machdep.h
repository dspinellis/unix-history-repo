/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)machdep.h	5.2 (Berkeley) 11/12/86
 */

#ifdef ADDR32
#define pushaddr push4
#define popaddr (char *)pop4
#endif ADDR32
#ifdef ADDR16
#define pushaddr push2
#define popaddr (char *)pop2
#endif ADDR16

/*
 * Machine specific macros for reading quantities from the
 * interpreter instruction stream. Operands in the instruction
 * stream are aligned to short, but not long boundries. Blockmarks
 * are always long aligned. Stack alignment indicates whether the
 * stack is short or long aligned. Stack alignment is assumed to
 * be no more than long aligned for ADDR32 machines, short aligned
 * for ADDR16 machines.
 */
#if defined(vax) || defined(mc68000) || defined(pdp11)
#define PCLONGVAL(target) target = *pc.lp++
#define GETLONGVAL(target, srcptr) target = *(long *)(srcptr)
#define STACKALIGN(target, value) target = ((value) + 1) &~ 1
#endif vax || mc68000 || pdp11

#ifdef tahoe
#define PCLONGVAL(target) target = *pc.sp++ << 16, target += *pc.usp++
#define GETLONGVAL(target, srcptr) \
	tsp = (short *)(srcptr), \
	target = *tsp++ << 16, target += *(unsigned short *)tsp
#define STACKALIGN(target, value) target = ((value) + 3) &~ 3
#endif tahoe
