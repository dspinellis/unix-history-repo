/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)alloca.s	5.2 (Berkeley) %G%"
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
