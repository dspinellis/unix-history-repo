/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigprocmask.s	5.3 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

err:
	jmp	cerror

ENTRY(sigprocmask)
	tstl	8(ap)			# check new sigset pointer
	bneq	1f			# if not null, indirect
/*	movl	$0,8(ap)		# null mask pointer: block empty set */
	movl	$1,4(ap)		# SIG_BLOCK
	jbr	2f
1:	movl	*8(ap),8(ap)		# indirect to new mask arg
2:	chmk	$SYS_sigprocmask
	jcs	err
	tstl	12(ap)			# test if old mask requested
	beql	out
	movl	r0,*12(ap)		# store old mask
out:
	clrl	r0
	ret
