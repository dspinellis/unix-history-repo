/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)execl.s	5.6 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(execl)
	pushab	8(ap)
	pushl	4(ap)
	calls	$2,_execv
	ret		# execl(file, arg1, arg2, ..., 0);
