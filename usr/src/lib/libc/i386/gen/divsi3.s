/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)divsi3.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

	.globl ___divsi3
___divsi3:
	movl 4(%esp),%eax
	cltd
	idivl 8(%esp)
	ret
