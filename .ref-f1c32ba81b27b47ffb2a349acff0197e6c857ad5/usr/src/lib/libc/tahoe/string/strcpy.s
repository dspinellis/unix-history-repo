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
	.asciz "@(#)strcpy.s	1.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Copy string s2 to s1.  s1 must be large enough.
 * return s1
 */
#include "DEFS.h"

ENTRY(strcpy, 0)
	movl	4(fp),r1
	movl	r1,r2
	movl	8(fp),r0
	movs2
	movl	r2,r0
	ret
