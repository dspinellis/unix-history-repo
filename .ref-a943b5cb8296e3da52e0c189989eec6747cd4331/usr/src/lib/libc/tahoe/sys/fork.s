/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)fork.s	5.1 (Berkeley) %G%"
#endif SYSLIBC_SCCS

#include "SYS.h"

SYSCALL(fork)
	bitl	$1,r1
	beql	1f	# parent, since r1 == 0 in parent, 1 in child
	clrl	r0
1:
	ret		# pid = fork()
