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
	.asciz "@(#)alloca.s	5.1 (Berkeley) 5/12/90"
#endif /* LIBC_SCCS and not lint */

/* like alloc, but automatic free in return */

#include "DEFS.h"

ENTRY(alloca)
	movl	sp@,a0		/* save return addr */
	movl	sp,d0		/* get current SP value */
	subl	sp@(4),d0	/* allocate requested space */
	andb	#~3,d0		/* longword align for efficiency */
	addql	#8,d0		/* reuse space of call frame */
	movl	d0,sp		/* set new SP value */
	lea	sp@(-4),sp	/* account for argument pop in caller */
	jmp 	a0@		/* funny return */
