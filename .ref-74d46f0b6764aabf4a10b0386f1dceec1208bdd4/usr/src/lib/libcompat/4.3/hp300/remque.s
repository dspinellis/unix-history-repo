/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)remque.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* remque(entry) */

#include "DEFS.h"

ENTRY(remque)
	movl	sp@(4),a0
	movl	a0@,a1
	movl	a0@(4),a0
	movl	a0,a1@(4)
	movl	a1,a0@
	rts
