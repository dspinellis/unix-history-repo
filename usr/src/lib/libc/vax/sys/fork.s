/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)fork.s	5.2 (Berkeley) %G%"
#endif not lint

#include "SYS.h"

SYSCALL(fork)
	jlbc	r1,1f	# parent, since r1 == 0 in parent, 1 in child
	clrl	r0
1:
	ret		# pid = fork()
