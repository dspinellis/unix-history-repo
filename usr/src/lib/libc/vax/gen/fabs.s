/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fabs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* fabs - floating absolute value */

#include "DEFS.h"

ENTRY(fabs, 0)
	movd	4(ap),r0
	bgeq	1f
	mnegd	r0,r0
1:
	ret
