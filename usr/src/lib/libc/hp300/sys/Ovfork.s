/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)Ovfork.s	5.1 (Berkeley) 5/12/90"
#endif /* LIBC_SCCS and not lint */

/*
 * @(#)vfork.s	4.1 (Berkeley) 12/21/80
 * C library -- vfork
 */

/*
 * pid = vfork();
 *
 * d1 == 0 in parent process, d1 == 1 in child process.
 * d0 == pid of child in parent, d0 == pid of parent in child.
 *
 * trickery here, due to keith sklower, uses ret to clear the stack,
 * and then returns with a jump indirect, since only one person can return
 * with a ret off this stack... we do the ret before we vfork!
 */

	vfork = 66
.globl	_vfork

_vfork:
	movl	sp@+,a0
	movl	#vfork,d0
	trap	#0
	jcc	vforkok
	jmp	verror
vforkok:
	tstl	d1		/* child process ? */
	jne	child		/* yes */
	jcc 	parent		/* if c-bit not set, fork ok */
	.globl	_errno
verror:
	movl	d0,_errno
	moveq	#-1,d0
	jmp	a0@
child:
	clrl	d0
parent:
	jmp	a0@
