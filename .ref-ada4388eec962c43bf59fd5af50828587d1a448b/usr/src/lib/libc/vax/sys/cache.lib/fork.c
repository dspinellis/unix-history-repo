/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fork.c	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.globl	mypid, myppid

SYSCALL(fork)
	jlbc	r1,1f	# parent, since r1 == 0 in parent, 1 in child
	movl	r0,myppid
	clrl	mypid
	clrl	r0
1:
	ret		# pid = fork()
