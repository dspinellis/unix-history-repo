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
 * from: $Header: strlen.s,v 1.1 92/06/25 12:52:47 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strlen.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(strlen)
	add	%o0, 1, %o1	! save starting point + 1
1:
	ldsb	[%o0], %o2	! fetch byte
	tst	%o2		! null?
	bne	1b		! no, keep going
	inc	%o0		! always increment pointer
	retl
	sub	%o0, %o1, %o0	! return length (ptr - (origptr+1))
