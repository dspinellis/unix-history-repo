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
	ASMSTR("@(#)Ovfork.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/*
 * pid = vfork();
 *
 * v1 == 0 in parent process, v1 == 1 in child process.
 * v0 == pid of child in parent, v0 == pid of parent in child.
 */

LEAF(vfork)
	li	v0, SYS_vfork		# system call number for vfork
	syscall
	beq	a3, zero, 1f		# jump if no errors
	j	_cerror
1:
	beq	v1, zero, 2f		# parent process ?
	move	v0, zero		# return zero in child
2:
	j	ra
END(vfork)
