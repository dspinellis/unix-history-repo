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
 * from: $Header: cerror.s,v 1.3 92/07/02 04:17:59 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)cerror.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

	.globl	_errno
FUNC(cerror)
	sethi	%hi(_errno), %g1
	st	%o0, [%g1 + %lo(_errno)]
	mov	-1, %o0
	retl
	 mov	-1, %o1
