/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)exect.s	5.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"
#include <machine/psl.h>

ENTRY(exect)
	lea	SYS_execve,%eax
	pushf
	popl	%edx
	orl	$ PSL_T,%edx
	pushl	%edx
	popf
	LCALL(7,0)
	jmp	cerror		/* exect(file, argv, env); */
