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
	.asciz "@(#)index.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(index)
	movl	sp@(4),a0	/* string */
	movb	sp@(11),d0	/* char to look for */
ixloop:
	cmpb	a0@,d0		/* found our char? */
	jeq	ixfound		/* yes, break out */
	tstb	a0@+		/* null? */
	jne	ixloop		/* no, keep going */
	moveq	#0,d0		/* not found, return null */
	rts
ixfound:
	movl	a0,d0		/* found, return pointer */
	rts
