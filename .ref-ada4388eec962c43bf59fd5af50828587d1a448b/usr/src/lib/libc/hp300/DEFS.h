/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)DEFS.h	8.1 (Berkeley) %G%
 */

#ifdef __STDC__
#ifdef PROF
#if __GNUC__ >= 2
#define	ENTRY(x) \
	.globl _ ## x; .even; _ ## x:; .data; PROF ## x:; .long 0; .text; \
	link a6,\#0; lea PROF ## x,a0; jsr mcount; unlk a6
#else
#define	ENTRY(x) \
	.globl _ ## x; .even; _ ## x:; .data; PROF ## x:; .long 0; .text; \
	link a6,#0; lea PROF ## x,a0; jsr mcount; unlk a6
#endif
#else
#define	ENTRY(x) \
	.globl _ ## x; .even; _ ## x:
#endif
#else
#ifdef PROF
#define	ENTRY(x) \
	.globl _/**/x; .even; _/**/x:; .data; PROF/**/x:; .long 0; .text; \
	link a6,#0; lea PROF/**/x,a0; jsr mcount; unlk a6
#else
#define	ENTRY(x) \
	.globl _/**/x; .even; _/**/x:
#endif
#endif
