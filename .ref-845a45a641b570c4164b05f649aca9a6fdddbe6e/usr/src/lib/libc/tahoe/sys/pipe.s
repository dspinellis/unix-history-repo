/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)pipe.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

SYSCALL(pipe)
	movl	4(fp),r2
	movl	r0,(r2)
	movl	r1,4(r2)
	clrl	r0
	ret
