/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cerror.vax.s	5.2 (Berkeley) 1/12/88
 *
 * static char rcsid[] = "$Header: cerror.s,v 1.1 87/03/25 19:27:44 donn Rel $";
 *
 * modified version of cerror
 *
 * The idea is that every time an error occurs in a system call
 * I want a special function "syserr" called.  This function will
 * either print a message and exit or do nothing depending on
 * defaults and use of "onsyserr".
 */

.globl	cerror
.comm	_errno,4

cerror:
	movl	r0,_errno
	calls	$0,_syserr	/* new code */
	mnegl	$1,r0
	ret

.globl	__mycerror		/* clumsy way to get this loaded */

__mycerror:
	.word	0
	ret
