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
	ASMSTR("@(#)sigprocmask.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

LEAF(sigprocmask)	# sigprocmask(how, new, old) sigset_t *new, *old;
	bne	a1, zero, gotptr	# if new sigset pointer not null
	li	a0, 1			# how = SIG_BLOCK
	b	doit			# mask = zero
gotptr:
	lw	a1, 0(a1)		# indirect to new mask arg
doit:
	li	v0, SYS_sigprocmask
	syscall
	bne	a3, zero, err
	beq	a2, zero, out		# test if old mask requested
	sw	v0, 0(a2)		# store old mask
out:
	move	v0, zero
	j	ra
err:
	j	_cerror
END(sigprocmask)
