/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)Ovfork.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

/*
 * pid = vfork();
 *
 * %edx == 0 in parent process, %edx == 1 in child process.
 * %eax == pid of child in parent, %eax == pid of parent in child.
 *
 */
	.set	vfork,66
.globl	_vfork

_vfork:
	popl	%ecx		/* my rta into ecx */
	movl	$vfork, %eax
	LCALL(7,0)
	jb	verror
vforkok:
	cmpl	$0,%edx		/* child process? */
	jne	child		/* yes */
	jmp 	parent 
.globl	_errno
verror:
	movl	%eax,_errno
	movl	$-1,%eax
	jmp	%ecx
child:
	movl	$0,%eax
parent:
	jmp	%ecx
