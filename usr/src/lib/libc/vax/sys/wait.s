/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)wait.s	5.2 (Berkeley) %G%"
#endif not lint

#include "SYS.h"

SYSCALL(wait)
	tstl	4(ap)
	jeql	1f
	movl	r1,*4(ap)
1:
	ret
