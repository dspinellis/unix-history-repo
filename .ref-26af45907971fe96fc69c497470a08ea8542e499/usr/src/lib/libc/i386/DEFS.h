/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)DEFS.h	8.1 (Berkeley) %G%
 */

#ifdef PROF
#define	ENTRY(x)	.globl _/**/x; _/**/x:  \
			.data; 1:; .long 0; .text; lea 1b,%eax ; call mcount
#define	ASENTRY(x)	.globl x; x: \
			.data; 1:; .long 0; .text; lea 1b,%eax ; call mcount
#else
#define	ENTRY(x)	.globl _/**/x; _/**/x: 
#define	ASENTRY(x)	.globl x; x: 
#endif
