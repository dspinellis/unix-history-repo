/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)execle.s	5.1 (Berkeley) %G%";
#endif not lint

#include "SYS.h"

ENTRY(execle)
	movl	(ap),r0
	pushl	(ap)[r0]
	pushab	8(ap)
	pushl	4(ap)
	calls	$3,_execve
	ret		# execle(file, arg1, arg2, ..., env);
