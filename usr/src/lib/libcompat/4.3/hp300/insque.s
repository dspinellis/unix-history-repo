/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)insque.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque)
	movl	sp@(8),a0
	movl	sp@(4),a1
	movl	a0@,a1@
	movl	a0,a1@(4)
	movl	a1,a0@
	movl	a1@,a0
	movl	a1,a0@(4)
	rts
