/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)getgid.c	5.1 (Berkeley) %G%"
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
