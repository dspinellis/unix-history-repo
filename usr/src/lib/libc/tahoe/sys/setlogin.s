/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)setlogin.s	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

.globl	__logname_valid		/* in getlogin() */

SYSCALL(setlogin)
	movl	$0,__logname_valid
	ret			/* setlogin(name); */
