/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)socketpair.s	5.1 (Berkeley) %G%"
#endif SYSLIBC_SCCS

#include "SYS.h"

SYSCALL(socketpair)
	ret
