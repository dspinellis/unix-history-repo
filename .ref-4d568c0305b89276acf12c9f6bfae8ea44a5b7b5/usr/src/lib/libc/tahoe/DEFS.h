/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)DEFS.h	8.1 (Berkeley) %G%
 */

/*
 * Macros used to define entry points
 * in assembly language routines.
 */
#if defined(GPROF) || defined(PROF)
#define	ENTRY(name, regs) \
	.globl _/**/name; .align 2; _/**/name: .word regs; \
	.data; .align 2; 1: .long 0; .text; \
	pushal 1b; callf $8,mcount;
#define	ASENTRY(name, regs) \
	.globl name; .align 2; name: .word regs; \
	.data; .align 2; 1: .long 0; .text; \
	pushal 1b; callf $8,mcount;
#define	XENTRY(name, regs) \
	.globl _/**/name; .globl X/**/name; .align 2; \
	_/**/name: X/**/name: .word regs; \
	.data; .align 2; 1: .long 0; .text; \
	pushal 1b; callf $8,mcount
#else
#define	ENTRY(name, regs) \
	.globl _/**/name; .align 2; _/**/name: .word regs
#define	ASENTRY(name, regs) \
	.globl name; .align 2; name: .word regs
#define	XENTRY(name, regs) \
	.globl _/**/name; .globl X/**/name; .align 2; \
	_/**/name: X/**/name: .word regs;
#endif
#define R0	0x0001
#define R1	0x0002
#define R2	0x0004
#define R3	0x0008
#define R4	0x0010
#define R5	0x0020
#define R6	0x0040
#define	R7	0x0080
#define	R8	0x0100
#define	R9	0x0200
#define	R10	0x0400
#define	R11	0x0800
#define	R12	0x1000
