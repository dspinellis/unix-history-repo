/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)DEFS.h	5.1 (Berkeley) %G%
 */

#ifdef PROF
#ifdef __GNUC__
#define	ENTRY(x) \
	.globl _/**/x; .even; _/**/x:; .data; PROF/**/x:; .long 0; .text; \
	link a6,#0; lea PROF/**/x,a0; jsr mcount; unlk a6
#else
#define	ENTRY(x) \
	.globl _/**/x; .even; _/**/x:; .data; PROF/**/x:; .long 0; .text; \
	lea PROF/**/x,a0; jsr mcount
#endif
#else
#define	ENTRY(x) \
	.globl _/**/x; .even; _/**/x:
#endif
