/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strcpy.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(strcpy)
	movl	sp@(8),a0		/* a0 = fromaddr */
	movl	sp@(4),d0		/* return value is toaddr */
	movl	d0,a1			/* a1 = toaddr */
scloop:
	movb	a0@+,a1@+		/* copy a byte */
	jne	scloop			/* copied non-null, keep going */
	rts
