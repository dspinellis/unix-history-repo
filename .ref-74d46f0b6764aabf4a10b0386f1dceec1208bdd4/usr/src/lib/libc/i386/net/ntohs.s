/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ntohs.s	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* hostorder = ntohs(netorder) */

#include "DEFS.h"

ENTRY(ntohs)
	movzwl	4(%esp),%eax
	xchgb	%al,%ah
	ret
