/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)insque.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque, 0)
	insque	*4(fp), *8(fp)
	ret
