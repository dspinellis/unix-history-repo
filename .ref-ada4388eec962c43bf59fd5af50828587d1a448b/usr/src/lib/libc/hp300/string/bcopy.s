/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)bcopy.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * This is probably not the best we can do, but it is still 2-10 times
 * faster than the C version in the portable gen directory.
 *
 * Things that might help:
 *	- unroll the longword copy loop (might not be good for a 68020)
 *	- longword align when possible (only on the 68020)
 *	- use nested DBcc instructions or use one and limit size to 64K
 */
ENTRY(bcopy)
	movl	sp@(12),d1	/* check count */
	jle	bcdone		/* <= 0, don't do anything */
	movl	sp@(4),a0	/* src address */
	movl	sp@(8),a1	/* dest address */
	cmpl	a1,a0		/* src after dest? */
	jlt	bcback		/* yes, must copy backwards */
	movl	a0,d0
	btst	#0,d0		/* src address odd? */
	jeq	bcfeven		/* no, skip alignment */
	movb	a0@+,a1@+	/* yes, copy a byte */
	subql	#1,d1		/* adjust count */
	jeq	bcdone		/* count 0, all done  */
bcfeven:
	movl	a1,d0
	btst	#0,d0		/* dest address odd? */
	jne	bcfbloop	/* yes, no hope for alignment, copy bytes */
	movl	d1,d0		/* no, both even */
	lsrl	#2,d0		/* convert count to longword count */
	jeq	bcfbloop	/* count 0, skip longword loop */
bcflloop:
	movl	a0@+,a1@+	/* copy a longword */
	subql	#1,d0		/* adjust count */
	jne	bcflloop	/* still more, keep copying */
	andl	#3,d1		/* what remains */
	jeq	bcdone		/* nothing, all done */
bcfbloop:
	movb	a0@+,a1@+	/* copy a byte */
	subql	#1,d1		/* adjust count */
	jne	bcfbloop	/* still more, keep going */
bcdone:
	rts
bcback:
	addl	d1,a0		/* src pointer to end */
	addl	d1,a1		/* dest pointer to end */
	movl	a0,d0
	btst	#0,d0		/* src address odd? */
	jeq	bcbeven		/* no, skip alignment */
	movb	a0@-,a1@-	/* yes, copy a byte */
	subql	#1,d1		/* adjust count */
	jeq	bcdone		/* count 0, all done  */
bcbeven:
	movl	a1,d0
	btst	#0,d0		/* dest address odd? */
	jne	bcbbloop	/* yes, no hope for alignment, copy bytes */
	movl	d1,d0		/* no, both even */
	lsrl	#2,d0		/* convert count to longword count */
	jeq	bcbbloop	/* count 0, skip longword loop */
bcblloop:
	movl	a0@-,a1@-	/* copy a longword */
	subql	#1,d0		/* adjust count */
	jne	bcblloop	/* still more, keep copying */
	andl	#3,d1		/* what remains */
	jeq	bcdone		/* nothing, all done */
bcbbloop:
	movb	a0@-,a1@-	/* copy a byte */
	subql	#1,d1		/* adjust count */
	jne	bcbbloop	/* still more, keep going */
	rts

