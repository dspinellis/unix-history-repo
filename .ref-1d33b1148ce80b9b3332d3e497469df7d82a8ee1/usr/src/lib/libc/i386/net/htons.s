/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)htons.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* netorder = htons(hostorder) */

#include "DEFS.h"

ENTRY(htons)
	movzwl	4(%esp),%eax
	xchgb	%al,%ah
	ret
