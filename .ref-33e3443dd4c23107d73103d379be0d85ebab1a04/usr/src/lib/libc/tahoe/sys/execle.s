/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)execle.s	5.1 (Berkeley) %G%"
#endif SYSLIBC_SCCS

#include "SYS.h"

ENTRY(execle)
	movw	-2(fp),r0	# removed word.
	subw2	$4,r0
	shar	$2,r0,r0	# num. of args.
	pushl	(fp)[r0]
	pushab	8(fp)
	pushl	4(fp)
	calls	$16,_execve
	ret		# execle(file, arg1, arg2, ..., env);
