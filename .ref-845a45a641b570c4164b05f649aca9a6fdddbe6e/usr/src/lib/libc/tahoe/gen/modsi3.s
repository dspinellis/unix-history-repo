/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)modsi3.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(__modsi3, 0)
	clrl	r0
	movl	4(fp),r1
	jgeq	1f
	mnegl	$1,r0
1:	ediv	8(fp),r0,r1,r0
	ret
