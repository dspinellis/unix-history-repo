/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)execv.c	5.3 (Berkeley) 3/9/86"
#endif SYSLIBC_SCCS

#include "SYS.h"

ENTRY(execv)
	.globl	_environ
	pushl	_environ
	pushl	8(ap)
	pushl	4(ap)
	calls	$3,_execve
	ret			# execv(file, argv)
