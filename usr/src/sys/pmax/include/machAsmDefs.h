/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)machAsmDefs.h	7.6 (Berkeley) %G%
 */

/*
 * machAsmDefs.h --
 *
 *	Macros used when writing assembler programs.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/mach/ds3100.md/RCS/machAsmDefs.h,
 *	v 1.2 89/08/15 18:28:24 rab Exp $ SPRITE (DECWRL)
 */

#ifndef _MACHASMDEFS
#define _MACHASMDEFS

#include <machine/regdef.h>

/*
 * Define -pg profile entry code.
 */
#if defined(GPROF) || defined(PROF)
#define	MCOUNT	.set noreorder; \
		.set noat; \
		move $1,$31; \
		jal _mcount; \
		subu sp,sp,8; \
		.set reorder; \
		.set at;
#else
#define	MCOUNT
#endif

/*
 * LEAF(x)
 *
 *	Declare a leaf routine.
 */
#define LEAF(x) \
	.globl x; \
	.ent x, 0; \
x: ; \
	.frame sp, 0, ra; \
	MCOUNT

/*
 * NLEAF(x)
 *
 *	Declare a non-profiled leaf routine.
 */
#define NLEAF(x) \
	.globl x; \
	.ent x, 0; \
x: ; \
	.frame sp, 0, ra

/*
 * ALEAF -- declare alternate entry to a leaf routine.
 */
#define	ALEAF(x)					\
	.globl	x;					\
	.aent	x,0;					\
x:

/*
 * NON_LEAF(x)
 *
 *	Declare a non-leaf routine (a routine that makes other C calls).
 */
#define NON_LEAF(x, fsize, retpc) \
	.globl x; \
	.ent x, 0; \
x: ; \
	.frame sp, fsize, retpc; \
	MCOUNT

/*
 * NNON_LEAF(x)
 *
 *	Declare a non-profiled non-leaf routine
 *	(a routine that makes other C calls).
 */
#define NNON_LEAF(x, fsize, retpc) \
	.globl x; \
	.ent x, 0; \
x: ; \
	.frame sp, fsize, retpc

/*
 * END(x)
 *
 *	Mark end of a procedure.
 */
#define END(x) \
	.end x

#define STAND_FRAME_SIZE	24
#define STAND_RA_OFFSET		20

/*
 * Macros to panic and printf from assembly language.
 */
#define PANIC(msg) \
	la	a0, 9f; \
	jal	panic; \
	MSG(msg)

#define	PRINTF(msg) \
	la	a0, 9f; \
	jal	printf; \
	MSG(msg)

#define	MSG(msg) \
	.rdata; \
9:	.asciiz	msg; \
	.text

#define ASMSTR(str) \
	.asciiz str; \
	.align	2

#endif /* _MACHASMDEFS */
