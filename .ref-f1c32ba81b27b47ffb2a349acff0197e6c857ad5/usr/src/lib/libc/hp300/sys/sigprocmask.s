/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigprocmask.s	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.even
err:
	jmp	cerror

ENTRY(sigprocmask)
	tstl	sp@(8)			/* check new sigset pointer */
	jne	gotptr			/* if not null, indirect */
/*	movl	#0,sp@(8)		/* null mask pointer: block empty set */
	movl	#1,sp@(4)		/* SIG_BLOCK */
	jra	doit
gotptr:
	movl	sp@(8),a0
	movl	a0@,sp@(8)		/* indirect to new mask arg */
doit:
	movl	#SYS_sigprocmask,d0
	trap	#0
	jcs	err
	tstl	sp@(12)			/* test if old mask requested */
	jeq	out
	movl	sp@(12),a0
	movl	d0,a0@			/* store old mask */
out:
	clrl	d0
	rts
