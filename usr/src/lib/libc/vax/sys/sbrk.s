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
_sccsid:.asciz	"@(#)sbrk.s	5.4 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

#define	SYS_brk		17

	.globl	_end
	.globl	minbrk
	.globl	curbrk

	.data
minbrk: .long	_end
curbrk:	.long	_end
	.text

ENTRY(sbrk)
	addl3	curbrk,4(ap),-(sp)
	pushl	$1
	movl	ap,r3
	movl	sp,ap
	chmk	$SYS_brk
	jcs 	err
	movl	curbrk,r0
	addl2	4(r3),curbrk
	ret
err:
	jmp	cerror
