/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cerror.iris.s	5.1 (Berkeley) %G%
 *
 * static char rcsid[] = "$Header: cerror.s,v 1.2 87/03/25 19:32:31 donn Exp $";
 *
 * modified version of cerror
 *
 * The idea is that every time an error occurs in a system call
 * I want a special function "syserr" called.  This function will
 * either print a message and exit or do nothing depending on
 * defaults and use of "onsyserr".
 */

.comm	_errno,4

.globl	cerror
cerror:
	movl	d0,_errno
	jbsr	_syserr		/* new code */
	moveq	#-1,d0
	rts

.globl	__mycerror		/* clumsy way to get this loaded */

__mycerror:
	rts
