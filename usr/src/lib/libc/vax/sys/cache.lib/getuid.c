/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)getuid.c	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.data
myuid:	.long	-1
myeuid:	.long	-1
	.text

ENTRY(getuid)
	movl	myuid,r0	# check cache
	cmpl	$-1,r0
	bneq	doit
	ret
doit:
	chmk	$SYS_getuid
	jcs	err
	movl	r0,myuid	# set cache
	movl	r1,myeuid	# set cache
	ret			# uid = getuid();

ENTRY(geteuid)
	movl	myeuid,r0	# check cache
	cmpl	$-1,r0
	bneq	doit
	ret
doit:
	chmk	$SYS_getuid
	jcs	err
	movl	r0,myuid	# set cache
	movl	r1,r0
	movl	r0,myeuid	# set cache
	ret			# uid = geteuid();
err:
	jmp cerror

ENTRY(setreuid)
	mnegl	$1,myuid
	mnegl	$1,myeuid
	chmk	$SYS_setreuid
	jcs	err
	ret		# setreuid(ruid, euid)
