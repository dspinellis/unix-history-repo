/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)execv.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(execv)
	.globl	_environ
	pushl	_environ
	pushl	8(fp)
	pushl	4(fp)
	calls	$16,_execve
	ret			# execv(file, argv)
