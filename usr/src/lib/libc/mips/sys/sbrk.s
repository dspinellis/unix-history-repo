/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include "SYS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)sbrk.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

#define	SYS_brk		17

	.data
	.globl	minbrk
minbrk:
	.word	end
	.globl	curbrk
curbrk:
	.word	end
	.text

LEAF(sbrk)
	lw	v1, curbrk
	li	v0, SYS_brk
	addu	a0, a0, v1	# compute current break
	syscall
	bne	a3, zero, 1f
	move	v0, v1		# return old val of _curbrk from above
	sw	a0, curbrk	# save current val of _curbrk from above
	j	ra
1:
	j	_cerror
END(sbrk)
