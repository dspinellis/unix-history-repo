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
	ASMSTR("@(#)index.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(index)
	lbu	a2, 0(a0)		# get a byte
	addu	a0, a0, 1
	beq	a2, a1, fnd
	bne	a2, zero, index
notfnd:
	move	v0, zero
	j	ra
fnd:
	subu	v0, a0, 1
	j	ra
END(index)
