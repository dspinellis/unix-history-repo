/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include <machine/machAsmDefs.h>

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)rindex.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(rindex)
	move	v0, zero		# default if not found
1:
	lbu	a3, 0(a0)		# get a byte
	addu	a0, a0, 1
	bne	a3, a1, 2f
	subu	v0, a0, 1		# save address of last match
2:
	bne	a3, zero, 1b		# continue if not end
	j	ra
END(rindex)
