/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)execl.s	5.2 (Berkeley) %G%"
#endif not lint

#include "SYS.h"

ENTRY(execl)
	pushab	8(ap)
	pushl	4(ap)
	calls	$2,_execv
	ret		# execl(file, arg1, arg2, ..., 0);
