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
	.asciz "@(#)strlen.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(strlen)
	moveq	#-1,d0
	movl	sp@(4),a0	/* string */
slloop:
	addql	#1,d0		/* increment count */
	tstb	a0@+		/* null? */
	jne	slloop		/* no, keep going */
	rts
