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
	.asciz "@(#)insque.s	1.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque, 0)
	insque	*4(fp), *8(fp)
	ret
