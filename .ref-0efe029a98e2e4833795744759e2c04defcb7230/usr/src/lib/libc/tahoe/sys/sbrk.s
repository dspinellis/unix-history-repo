/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sbrk.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

#define	SYS_brk		17

	.globl	_end
	.globl	minbrk
	.globl	curbrk

	.data
minbrk: .long	_end
curbrk:	.long	_end
	.text

ENTRY(sbrk)
	addl3	curbrk,4(fp),-(sp)
	pushl	$1
	movl	fp,r3
	moval	(sp),fp
	kcall	$SYS_brk
	jcs 	err
	movl	curbrk,r0
	addl2	4(r3),curbrk
	movl	r3,fp
	ret
err:
	movl	r3,fp
	jmp	cerror
