/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
_sccsid:.asciz	"@(#)brk.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

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
