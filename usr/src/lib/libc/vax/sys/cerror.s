/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)cerror.s	5.2 (Berkeley) %G%"
#endif not lint

#include "SYS.h"

	.globl	_errno
cerror:
	movl	r0,_errno
	mnegl	$1,r0
	ret
