/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)execle.c	5.3 (Berkeley) 3/9/86"
#endif SYSLIBC_SCCS

#include "SYS.h"

ENTRY(execle)
	movl	(ap),r0
	pushl	(ap)[r0]
	pushab	8(ap)
	pushl	4(ap)
	calls	$3,_execve
	ret		# execle(file, arg1, arg2, ..., env);
