/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)abs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* abs - int absolute value */

#include "DEFS.h"

ENTRY(abs, 0)
	movl	4(fp),r0
	bgeq	1f
	mnegl	r0,r0
1:
	ret
