/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)ptrace.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(ptrace)
	clrl	_errno
	kcall	$SYS_ptrace
	jcs	err
	ret
err:
	jmp	cerror
