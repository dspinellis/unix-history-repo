/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)ptrace.s	5.1 (Berkeley) %G%"
#endif SYSLIBC_SCCS

#include "SYS.h"

ENTRY(ptrace)
	clrl	_errno
	kcall	$SYS_ptrace
	jcs	err
	ret
err:
	jmp	cerror
