/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)reboot.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

SYSCALL(reboot)
	halt
