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
	.asciz "@(#)brk.s	5.4 (Berkeley) 6/1/90"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

#define	SYS_brk		17

	.globl	curbrk
	.globl	minbrk

ENTRY(brk)
	cmpl	4(fp),minbrk
	bgeq	ok
	movl	minbrk,4(fp)
ok:
	kcall	$SYS_brk
	jcs	err
	movl	4(fp),curbrk
	clrl	r0
	ret
err:
	jmp	cerror
