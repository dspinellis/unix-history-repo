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
	.asciz "@(#)bzero.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * This is probably not the best we can do, but it is still much
 * faster than the C version in the portable gen directory.
 *
 * Things that might help:
 *	- unroll the longword loop (might not be good for a 68020)
 *	- longword, as opposed to word, align when possible (only on the 68020)
 *	- use nested DBcc instructions or use one and limit size to 64K
 */
ENTRY(bzero)
	movl	sp@(4),a0	/* destination */
	movl	sp@(8),d0	/* count */
	jeq	bzdone		/* nothing to do */
	movl	a0,d1
	btst	#0,d1		/* address odd? */
	jeq	bzeven		/* no, skip alignment */
	clrb	a0@+		/* yes, clear a byte */
	subql	#1,d0		/* adjust count */
	jeq	bzdone		/* if zero, all done */
bzeven:
	movl	d0,d1
	lsrl	#2,d1		/* convert to longword count */
	jeq	bzbloop		/* no longwords, skip loop */
bzlloop:
	clrl	a0@+		/* clear a longword */
	subql	#1,d1		/* adjust count */
	jne	bzlloop		/* still more, keep going */
	andl	#3,d0		/* what remains */
	jeq	bzdone		/* nothing, all done */
bzbloop:
	clrb	a0@+		/* clear a byte */
	subql	#1,d0		/* adjust count */
	jne	bzbloop		/* still more, keep going */
bzdone:
	rts
