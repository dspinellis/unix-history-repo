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
	ASMSTR("@(#)reboot.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(reboot)
	li	v0, SYS_reboot
	syscall
	bne	a3, zero, 1f
	jal	abort
1:
	j	_cerror
END(reboot)
