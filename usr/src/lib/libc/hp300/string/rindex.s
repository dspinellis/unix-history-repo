/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)rindex.s	5.1 (Berkeley) 5/12/90"
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
