/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: brk.s,v 1.3 92/06/25 12:56:05 mccanne Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)brk.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.globl	curbrk
	.globl	minbrk

	.data
minbrk:	.long	_end			! lower brk limit; also for gmon code
	.text

ENTRY(brk)
	sethi	%hi(minbrk), %o1	! %o1 = minbrk
	ld	[%o1 + %lo(minbrk)], %o1
	cmp	%o1, %o0		! if (minbrk > %o0)
	bg,a	0f
	 mov	%o1, %o0		!	%o0 = minbrk
0:
	mov	%o0, %o2		! save argument to syscall
	mov	SYS_break, %g1
	t	ST_SYSCALL
	bcc,a	1f
	 sethi	%hi(curbrk), %g1
	ERROR()
1:
	retl				! success, return 0 & record new break
	 st	%o2, [%g1 + %lo(curbrk)]
