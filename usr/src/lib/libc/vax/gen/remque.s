/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef LIBC_SCCS
	.asciz	"@(#)remque.s	5.3 (Berkeley) 3/9/86"
#endif LIBC_SCCS

/* remque(entry) */

#include "DEFS.h"

ENTRY(remque, 0)
	remque	*4(ap),r0
	ret
