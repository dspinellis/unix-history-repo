/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)syscall.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(syscall)
	pop	%ecx	/* rta */
	pop	%eax	/* syscall number */
	push	%ecx
	LCALL(7,0)
	jb	1f
	ret
1:
	jmp	cerror
