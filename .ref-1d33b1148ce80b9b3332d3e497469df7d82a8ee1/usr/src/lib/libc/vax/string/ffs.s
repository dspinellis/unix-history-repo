/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ffs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* bit = ffs(value) */

#include "DEFS.h"

ENTRY(ffs, 0)
	ffs	$0,$32,4(ap),r0
	bneq	1f
	mnegl	$1,r0
1:
	incl	r0
	ret
