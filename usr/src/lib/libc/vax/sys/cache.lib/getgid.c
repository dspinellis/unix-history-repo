/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)getgid.c	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.data
mygid:	.long	-1
myegid:	.long	-1
	.text

ENTRY(getgid)
	movl	mygid,r0	# check cache
	cmpl	$-1,r0
	bneq	doit
	ret
doit:
	chmk	$SYS_getgid
	jcs	err
	movl	r0,mygid	# set cache
	movl	r1,myegid	# set cache
	ret			# gid = getgid();

ENTRY(getegid)
	movl	myegid,r0	# check cache
	cmpl	$-1,r0
	bneq	doit
	ret
doit:
	chmk	$SYS_getgid
	jcs	err
	movl	r0,mygid	# set cache
	movl	r1,r0
	movl	r0,myegid	# set cache
	ret			# gid = getegid();
err:
	jmp cerror

ENTRY(setregid)
	mnegl	$1,mygid
	mnegl	$1,myegid
	chmk	$SYS_setregid
	jcs	err
	ret		# setregid(rgid, egid)
