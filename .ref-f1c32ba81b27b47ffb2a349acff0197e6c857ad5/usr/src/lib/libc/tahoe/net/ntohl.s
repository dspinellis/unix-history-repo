/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ntohl.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* hostorder = ntohl(netorder) */

#include "DEFS.h"

ENTRY(ntohl, 0)
	movl	4(fp),r0
	ret
