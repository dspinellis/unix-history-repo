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
	ASMSTR("@(#)brk.s	5.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

#define	SYS_brk		17

LEAF(brk)
	lw	v0, _minbrk
	bgeu	a0, v0, _brk
	move	a0, v0		# don't allow break < minbrk
ALEAF(_brk)
	li	v0, SYS_brk
	syscall
	bne	a3, zero, 1f
	sw	a0, _curbrk
	move	v0, zero
	j	ra
1:
	j	_cerror
END(brk)
