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
	.asciz "@(#)sigsuspend.s	5.2 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

err:
	jmp	cerror

ENTRY(sigsuspend)
	movl	4(%esp),%eax		# fetch mask arg
	movl	(%eax),%eax		# indirect to mask arg
	movl	%eax,4(%esp)
	movl	$ SYS_sigsuspend ,%eax
	LCALL(0x7,0)
	jb	err
	xorl	%eax,%eax		# shouldn t happen
	ret
