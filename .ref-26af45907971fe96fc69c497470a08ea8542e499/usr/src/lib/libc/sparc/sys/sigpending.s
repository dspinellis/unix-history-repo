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
 * from: $Header: sigpending.s,v 1.1 91/07/06 13:06:00 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigpending.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(sigpending)
	mov	%o0, %o2		! save pointer
	mov	SYS_sigpending, %g1
	t	ST_SYSCALL		! sigpending()
	bcc,a	1f			! if success,
	 st	%o0, [%o2]		!    store return value
	ERROR()
1:
	retl				! and return 0
	 clr	%o0
