/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)remque.s	1.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* remque(entry) */

#include "DEFS.h"

ENTRY(remque, 0)
	remque	*4(fp)
	ret
