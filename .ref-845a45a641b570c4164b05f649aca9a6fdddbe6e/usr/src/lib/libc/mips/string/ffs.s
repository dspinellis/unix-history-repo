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
	ASMSTR("@(#)ffs.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/* bit = ffs(value) */

LEAF(ffs)
	move	v0, zero
	beq	a0, zero, done
1:
	and	v1, a0, 1		# bit set?
	addu	v0, v0, 1
	srl	a0, a0, 1
	beq	v1, zero, 1b		# no, continue
done:
	j	ra
END(ffs)
