/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ptrace.s	5.1 (Berkeley) %G%";
#endif not lint

#include "SYS.h"

ENTRY(ptrace)
	clrl	_errno
	chmk	$SYS_ptrace
	jcs	err
	ret
err:
	jmp	cerror
