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
	.asciz "@(#)strncmp.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * NOTE: this guy returns result compatible with the VAX assembly version.
 * The C version on the portable gen directory returns different results
 * (different signs!) when comparing chars with the high bit on.  Who is
 * right??
 */
ENTRY(strncmp)
	movl	sp@(12),d1	/* count */
	jeq	scdone		/* nothing to do */
	movl	sp@(4),a0	/* a0 = string1 */
	movl	sp@(8),a1	/* a1 = string2 */
scloop:
	movb	a0@+,d0		/* get *string1 */
	cmpb	a1@+,d0		/* compare a byte */
	jne	scexit		/* not equal, break out */
	tstb	d0		/* at end of string1? */
	jeq	scdone		/* yes, all done */
	subql	#1,d1		/* no, adjust count */
	jne	scloop		/* more to do, keep going */
scdone:
	moveq	#0,d0		/* strings are equal */
	rts
scexit:
	subb	a1@-,d0		/* *string1 - *string2 */
	extbl	d0
	rts
