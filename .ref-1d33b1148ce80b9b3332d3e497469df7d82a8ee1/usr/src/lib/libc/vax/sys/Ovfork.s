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
 * @(#)vfork.s	4.1 (Berkeley) 12/21/80
 * C library -- vfork
 */

#include "SYS.h"

/*
 * pid = vfork();
 *
 * r1 == 0 in parent process, r1 == 1 in child process.
 * r0 == pid of child in parent, r0 == pid of parent in child.
 *
 * trickery here, due to keith sklower, uses ret to clear the stack,
 * and then returns with a jump indirect, since only one person can return
 * with a ret off this stack... we do the ret before we vfork!
 */

ENTRY(vfork)
	movl	16(fp),r2	# save return address before we smash it
	movab	here,16(fp)
	ret
here:
	chmk	$SYS_vfork
	bcs	err		# if failed, set errno and return -1
	/* this next trick is Chris Torek's fault */
	mnegl	r1,r1		# r1 = 0xffffffff if child, 0 if parent
	bicl2	r1,r0		# r0 &= ~r1, i.e., 0 if child, else unchanged
	jmp	(r2)

err:
	movl	r0,_errno
	mnegl	$1,r0
	jmp	(r2)
