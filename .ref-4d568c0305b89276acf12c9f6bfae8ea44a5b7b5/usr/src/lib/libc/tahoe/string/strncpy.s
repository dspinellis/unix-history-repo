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
	.asciz "@(#)strncpy.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Copy s2 to s1, truncating or null-padding to always copy n bytes
 * return s1
 */
#include "DEFS.h"

ENTRY(strncpy, 0)
	movl	4(fp),r1
	movl	8(fp),r0
	movl	12(fp),r2
	movl	r1,r3
	movs3
	movl	r3,r0
	ret
