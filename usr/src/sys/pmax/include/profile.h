/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)profile.h	7.5 (Berkeley) %G%
 */

#define	_MCOUNT_DECL static void __mcount

#define	MCOUNT \
	asm(".globl _mcount;" \
	"_mcount:;" \
	".set noreorder;" \
	".set noat;" \
	"sw $4,8($29);" \
	"sw $5,12($29);" \
	"sw $6,16($29);" \
	"sw $7,20($29);" \
	"sw $1,0($29);" \
	"sw $31,4($29);" \
	"move $5,$31;" \
	"jal __mcount;" \
	"move $4,$1;" \
	"lw $4,8($29);" \
	"lw $5,12($29);" \
	"lw $6,16($29);" \
	"lw $7,20($29);" \
	"lw $31,4($29);" \
	"lw $1,0($29);" \
	"addu $29,$29,8;" \
	"j $31;" \
	"move $31,$1;" \
	".set reorder;" \
	".set at");

#ifdef KERNEL
/*
 * The following two macros do splhigh and splx respectively.
 * They have to be defined this way because these are real
 * functions on the PMAX, and we do not want to invoke mcount
 * recursively.
 */
#define	MCOUNT_ENTER	s = _splhigh()

#define	MCOUNT_EXIT	_splx(s)
#endif /* KERNEL */
