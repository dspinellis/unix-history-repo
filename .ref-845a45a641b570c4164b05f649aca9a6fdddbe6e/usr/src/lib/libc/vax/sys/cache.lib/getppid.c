/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)getppid.c	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.data
	.globl	mypid, myppid
myppid:
	.long	0
	.text

ENTRY(getpid)
	movl	myppid,r0	# check cache
	beql	doit
	ret
doit:
	chmk	$SYS_getpid
	jcs	err
	movl	r0,mypid	# cache pid
	movl	r1,r0
	movl	r0,myppid	# set cache
	ret			# ppid = getppid();
err:
	jmp cerror;
