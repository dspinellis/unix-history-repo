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
	ASMSTR("@(#)sigsuspend.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(sigsuspend)
	lw	a0, 0(a0)		# indirect to mask arg
	li	v0, SYS_sigsuspend
	syscall
	bne	a3, zero, 1f
	move	v0, zero		# should not happen
	j	ra
1:
	j	_cerror
END(sigsuspend)
