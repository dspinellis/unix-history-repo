/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include "SYS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)cerror.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

	.globl	errno
LEAF(_cerror)
	sw	v0, errno
	li	v0, -1
	j	ra
END(_cerror)
