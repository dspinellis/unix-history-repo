/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
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

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sbrk.s	5.6 (Berkeley) 6/1/90"
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
