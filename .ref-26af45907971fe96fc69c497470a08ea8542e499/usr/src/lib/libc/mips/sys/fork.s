/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include "SYS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)fork.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(fork)
	li	v0, SYS_fork	# pid = fork()
	syscall
	bne	a3, zero, 2f
	beq	v1, zero, 1f	# v1 == 0 in parent, 1 in child
	move	v0, zero
1:
	j	ra
2:
	j	_cerror
END(fork)
