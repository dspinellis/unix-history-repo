/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
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
	.asciz "@(#)alloca.s	5.2 (Berkeley) 5/14/90"
#endif /* LIBC_SCCS and not lint */

/* like alloc, but automatic automatic free in return */

#include "DEFS.h"

ENTRY(alloca)
	popl	%edx		/*  pop return addr */
	popl	%eax		/*  pop amount to allocate */
	movl	%esp,%ecx
	addl	$3,%eax		/*  round up to next word */
	andl	$0xfffffffc,%eax
	subl	%eax,%esp
	movl	%esp,%eax	/* base of newly allocated space */
	pushl	8(%ecx)		/* copy possible saved registers */
	pushl	4(%ecx)
	pushl	0(%ecx)
	pushl	%eax		/* dummy to pop at callsite */
	jmp	%edx		/* "return" */
