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
	.asciz "@(#)strncpy.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(strncpy)
	movl	sp@(4),d0	/* return value is toaddr */
	movl	sp@(12),d1	/* count */
	jeq	scdone		/* nothing to do */
	movl	sp@(8),a0	/* a0 = fromaddr */
	movl	d0,a1		/* a1 = toaddr */
scloop:
	movb	a0@+,a1@+	/* copy a byte */
	jeq	scploop		/* copied null, go pad if necessary */
	subql	#1,d1		/* adjust count */
	jne	scloop		/* more room, keep going */
scdone:
	rts
scploop:
	subql	#1,d1		/* adjust count */
	jeq	scdone		/* no more room, all done */
	clrb	a1@+		/* clear a byte */
	jra	scploop		/* keep going */
