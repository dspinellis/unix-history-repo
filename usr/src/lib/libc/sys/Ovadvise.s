/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)Ovadvise.s	5.3 (Berkeley) %G%"
#endif SYSLIBC_SCCS

#include "SYS.h"

#define	SYS_vadvise	72

SYSCALL(vadvise)
	ret
