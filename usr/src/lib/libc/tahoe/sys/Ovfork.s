/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)Ovfork.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

/*
 * C library -- vfork
 * pid = vfork();
 *
 * r1 == 0 in parent process, r1 == 1 in child process.
 * r0 == pid of child in parent, r0 == pid of parent in child.
 *
 * trickery here, due to keith sklower, uses ret to clear the stack,
 * and then returns with a jump indirect, since only one person can return
 * with a ret off this stack... we do the ret before we vfork!
 * 
 */

	.set	vfork,66
.globl	_vfork

_vfork:
	.word	0x0000
	movl	-8(fp),r2
	movab	here,-8(fp)
	ret
here:
	kcall	$vfork
	bcc	vforkok
	jmp	verror
vforkok:
	tstl	r1		# child process ?
	bneq	child	# yes
	jbr 	parent
.globl	_errno
verror:
	movl	r0,_errno
	mnegl	$1,r0
	jmp	(r2)
child:
	clrl	r0
parent:
	jmp	(r2)
