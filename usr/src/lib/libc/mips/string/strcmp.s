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
	ASMSTR("@(#)strcmp.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/*
 * NOTE: this version assumes unsigned chars in order to be "8 bit clean".
 */
LEAF(strcmp)
1:
	lbu	t0, 0(a0)		# get two bytes and compare them
	lbu	t1, 0(a1)
	beq	t0, zero, LessOrEq	# end of first string?
	bne	t0, t1, NotEq
	lbu	t0, 1(a0)		# unroll loop
	lbu	t1, 1(a1)
	add	a0, a0, 2
	beq	t0, zero, LessOrEq	# end of first string?
	add	a1, a1, 2
	beq	t0, t1, 1b
NotEq:
	subu	v0, t0, t1
	j	ra
LessOrEq:
	subu	v0, zero, t1
	j	ra
END(strcmp)
