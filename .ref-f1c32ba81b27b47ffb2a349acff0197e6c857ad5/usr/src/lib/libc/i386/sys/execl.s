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
	.asciz "@(#)execl.s	5.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(execl)
	lea	8(%esp),%eax
	pushl	%eax
	movl	4+4(%esp),%eax
	pushl	%eax
	call	_execv
	addl	$8,%esp
	ret			/* execl(file, arg1, arg2, ..., 0); */
