/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#include "SYS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR "@(#)reboot.s	5.6 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

SYSCALL(reboot)
	halt
