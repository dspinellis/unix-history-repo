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
 * from: $Header: sbrk.s,v 1.3 92/07/02 00:56:49 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sbrk.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.globl	_end
	.globl	curbrk

	.data
curbrk:	.long	_end
	.text

ENTRY(sbrk)
	sethi	%hi(curbrk), %o2
	ld	[%o2 + %lo(curbrk)], %o3	! %o3 = old break
	add	%o3, %o0, %o4			! %o4 = new break
	mov	%o4, %o0			! copy for syscall
	mov	SYS_break, %g1
	t	ST_SYSCALL			! break(new_break)
	bcc,a	1f				! if success,
	 mov	%o3, %o0			!    set return value
	ERROR()
1:
	retl					! and update curbrk
	 st	%o4, [%o2 + %lo(curbrk)]
