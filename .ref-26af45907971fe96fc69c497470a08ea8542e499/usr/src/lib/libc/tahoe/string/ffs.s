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
	.asciz "@(#)ffs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* bit = ffs(value) */

#include "DEFS.h"

ENTRY(ffs, 0)
	ffs	4(fp),r0
	bgeq	1f
	mnegl	$1,r0
1:
	incl	r0
	ret
