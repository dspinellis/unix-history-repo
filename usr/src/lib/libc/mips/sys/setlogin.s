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
	ASMSTR("@(#)setlogin.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(setlogin)
	li	v0, SYS_setlogin	# setlogin(name)
	syscall
	bne	a3, zero, 1f
	sw	zero, _logname_valid	# in getlogin()
	j	ra
1:
	j	_cerror
END(setlogin)
