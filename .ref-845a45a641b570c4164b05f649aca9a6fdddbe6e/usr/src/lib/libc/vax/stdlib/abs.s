/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)abs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(abs, 0)
	movl	4(ap),r0
	jgeq	1f
	mnegl	r0,r0
1:
	ret
