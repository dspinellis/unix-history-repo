/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)gethostid.c	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

	.data
hostid:
	.long	0
	.text

ENTRY(gethostid)
	movl	hostid,r0	# check cache
	beql	doit
	ret
doit:
	chmk	$SYS_gethostid
	jcs	err
	movl	r0,hostid	# set cache
	ret			# hostid = gethostid();
err:
	jmp cerror;
