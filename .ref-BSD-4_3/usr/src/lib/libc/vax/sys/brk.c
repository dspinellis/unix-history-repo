/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef SYSLIBC_SCCS
_sccsid:.asciz	"@(#)brk.c	5.3 (Berkeley) 3/9/86"
#endif SYSLIBC_SCCS

#include "SYS.h"

#define	SYS_brk		17

	.globl	curbrk
	.globl	minbrk
ENTRY(_brk)
	jbr	ok

ENTRY(brk)
	cmpl	4(ap),minbrk
	bgeq	ok
	movl	minbrk,4(ap)
ok:
	chmk	$SYS_brk
	jcs	err
	movl	4(ap),curbrk
	clrl	r0
	ret
err:
	jmp	cerror
