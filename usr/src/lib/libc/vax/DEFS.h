/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)DEFS.h	5.1 (Berkeley) %G%
 */

#define R0	0x001
#define R1	0x002
#define R2	0x004
#define R3	0x008
#define R4	0x010
#define R5	0x020
#define R6	0x040
#define	R7 	0x080
#define	R8	0x100
#define	R9	0x200
#define	R10	0x400
#define	R11	0x800

#ifdef PROF
#define	ENTRY(x, regs) \
	.globl _/**/x; .align 2; _/**/x: .word regs; \
	.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount
#define	ASENTRY(x, regs) \
	.globl x; .align 2; x: .word regs; \
	.data; 1:; .long 0; .text; moval 1b,r0; jsb mcount
#else
#define	ENTRY(x, regs) \
	.globl _/**/x; .align 2; _/**/x: .word regs
#define	ASENTRY(x, regs) \
	.globl x; .align 2; x: .word regs
#endif
