/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)execle.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

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
