/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: Ovfork.s,v 1.1 91/07/06 13:05:56 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)Ovfork.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * pid = vfork();
 *
 * %o1 == 0 in parent process, 1 in child process.
 * %o0 == pid of child in parent, pid of parent in child.
 */

#include "SYS.h"

SYSCALL(vfork)
	dec	%o1		! from 1 to 0 in child, 0 to -1 in parent
	retl
	and	%o0, %o1, %o0	! return 0 in child, pid in parent
