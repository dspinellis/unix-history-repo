/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef LIBC_SCCS
	.asciz	"@(#)insque.s	5.3 (Berkeley) 3/9/86"
#endif LIBC_SCCS

/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque, 0)
	insque	*4(ap), *8(ap)
	ret
