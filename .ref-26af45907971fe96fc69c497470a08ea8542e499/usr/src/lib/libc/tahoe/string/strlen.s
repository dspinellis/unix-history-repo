/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strlen.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Returns the number of
 * non-NULL bytes in string argument.
 */

#include "DEFS.h"

ENTRY(strlen, 0)
	movl	4(fp),r0
	movl	r0,r1
	cmps2
	subl2	4(fp),r0
	ret
