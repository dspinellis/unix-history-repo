/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)DEFS.h	5.1 (Berkeley) %G%
 */

#ifdef PROF
#define	ENTRY(x)	.globl _/**/x; .align 2; _/**/x: .word 0; \
			.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount
#define	ASENTRY(x)	.globl x; .align 2; x: .word 0; \
			.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount
#else
#define	ENTRY(x)	.globl _/**/x; .align 2; _/**/x: .word 0
#define	ASENTRY(x)	.globl x; .align 2; x: .word 0
#endif
