/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)execle.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(execle)
	lea	sp@(4),a0
	movl	a0,a1
again:
	tstl	a0@+
	jne	again
	movl	a0@,sp@-
	pea	a1@(4)
	movl	a1@,sp@-
	jbsr	_execve
	addl	#12,sp
	rts		/* execle(file, arg1, arg2, ..., env); */
