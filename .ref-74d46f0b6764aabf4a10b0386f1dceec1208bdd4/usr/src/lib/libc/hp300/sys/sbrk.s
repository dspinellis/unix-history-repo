/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sbrk.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

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
	movl	curbrk,d0
	addl	d0,sp@(4)
	movl	#SYS_brk,d0
	trap	#0
	jcs 	err
	movl	curbrk,d0
	movl	sp@(4),curbrk
	rts
err:
	jmp	cerror
