/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pipe.s	5.1 (Berkeley) %G%";
#endif not lint

#include "SYS.h"

SYSCALL(pipe)
	movl	4(ap),r2
	movl	r0,(r2)+
	movl	r1,(r2)
	clrl	r0
	ret
