/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)execv.s	5.1 (Berkeley) %G%"
#endif SYSLIBC_SCCS

#include "SYS.h"

ENTRY(execv)
	.globl	_environ
	pushl	_environ
	pushl	8(fp)
	pushl	4(fp)
	calls	$16,_execve
	ret			# execv(file, argv)
