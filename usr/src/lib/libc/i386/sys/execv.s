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
	.asciz "@(#)execv.s	5.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(execv)
	.globl	_environ
	pushl	_environ
	movl	8+4(%esp),%eax
	pushl	%eax
	movl	4+8(%esp),%eax
	pushl	%eax
	call	_execve
	addl	$12,%esp
	ret			/* execv(file, argv); */
