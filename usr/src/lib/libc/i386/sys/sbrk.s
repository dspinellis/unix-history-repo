/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sbrk.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

#define	SYS_brk		17

	.globl	_end
	.globl	minbrk
	.globl	curbrk

	.data
minbrk:	.long	_end
curbrk:	.long	_end
	.text

ENTRY(sbrk)
	movl	4(%esp),%ecx
	movl	curbrk,%eax
	addl	%eax,4(%esp)
	lea	SYS_brk,%eax
	LCALL(7,0)
	jb	err
	movl	curbrk,%eax
	addl	%ecx,curbrk
	ret
err:
	jmp	cerror
