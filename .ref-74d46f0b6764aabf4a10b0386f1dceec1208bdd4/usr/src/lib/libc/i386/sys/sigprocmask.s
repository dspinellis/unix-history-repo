/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sigprocmask.s	5.2 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

err:
	jmp	cerror

ENTRY(sigprocmask)
	movl	8(%esp),%ecx		# fetch new sigset pointer
	cmpl	$0,%ecx			# check new sigset pointer
	jne	1f			# if not null, indirect
/*	movl	$0,8(%esp)		# null mask pointer: block empty set */
	movl	$1,4(%esp)		# SIG_BLOCK
	jmp	2f
1:	movl	(%ecx),%ecx		# fetch indirect  ...
	movl	%ecx,8(%esp)		# to new mask arg
2:	movl	$ SYS_sigprocmask , %eax
	LCALL(0x7,0)
	jb	err
	movl	12(%esp),%ecx		# fetch old mask requested
	cmpl	$0,%ecx			# test if old mask requested
	je	out
	movl	%eax,(%ecx)		# store old mask
out:
	xorl	%eax,%eax
	ret
