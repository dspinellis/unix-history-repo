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
	.asciz "@(#)rindex.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(rindex)
	movl	sp@(4),a0	/* string */
	movb	sp@(11),d0	/* char to look for */
	subl	a1,a1		/* clear rindex pointer */
rixloop:
	cmpb	a0@,d0		/* found our char? */
	jne	rixnope		/* no, check for null */
	movl	a0,a1		/* yes, remember location */
rixnope:
	tstb	a0@+		/* null? */
	jne	rixloop		/* no, keep going */
	movl	a1,d0		/* return value */
	rts
