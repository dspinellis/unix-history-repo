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
	.asciz "@(#)fork.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

SYSCALL(fork)
	tstl	d1
	jeq	parent	/* parent, since d1 == 0 in parent, 1 in child */
	clrl	d0
parent:
	rts		/* pid = fork() */
