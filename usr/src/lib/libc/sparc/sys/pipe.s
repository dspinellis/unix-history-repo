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
 * from: $Header: pipe.s,v 1.1 91/07/06 13:05:58 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)pipe.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(pipe)
	mov	%o0, %o2	! save pointer
	mov	SYS_pipe, %g1
	t	ST_SYSCALL	! pipe()
	bcc,a	1f
	 st	%o0, [%o2]	! success, store fds
	ERROR()
1:
	st	%o1, [%o2 + 4]
	retl			! and return 0
	 clr	%o0
