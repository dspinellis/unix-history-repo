/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)pipe.s	5.6 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

SYSCALL(pipe)
	movl	4(ap),r2
	movl	r0,(r2)+
	movl	r1,(r2)
	clrl	r0
	ret
