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
	.asciz "@(#)brk.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

#define	SYS_brk		17

	.globl	curbrk
	.globl	minbrk
ENTRY(_brk)
	jmp	ok

ENTRY(brk)
	movl	4(%esp),%eax
	cmpl	%eax,minbrk
	jl	ok
	movl	minbrk,%eax
	movl	%eax,4(%esp)
ok:
	lea	SYS_brk,%eax
	LCALL(7,0)
	jb	err
	movl	4(%esp),%eax
	movl	%eax,curbrk
	movl	$0,%eax
	ret
err:
	jmp	cerror
