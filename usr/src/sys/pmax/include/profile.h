/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)profile.h	7.2 (Berkeley) %G%
 */

#define	_MCOUNT_DECL static void _mcount

#define	MCOUNT \
	asm(".globl mcount"); \
	asm("mcount:"); \
	asm(".set noreorder"); \
	asm(".set noat"); \
	asm("sw $31,4($29);"); \
	asm("jal _mcount"); \
	asm("sw $1,0($29);"); \
	asm("lw $31,4($29)"); \
	asm("lw $1,0($29)"); \
	asm("addu $29,$29,8"); \
	asm("j $31"); \
	asm("move $31,$1"); \
	asm(".set reorder"); \
	asm(".set at");

#ifdef KERNEL
/*
 * The following two macros do splhigh and splx respectively.
 * They have to be defined this way because these are real
 * functions on the PMAX, and we do not want to invoke mcount
 * recursively.
 */
#define	MCOUNT_ENTER

#define	MCOUNT_EXIT
#endif /* KERNEL */
