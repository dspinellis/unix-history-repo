/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
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
	.asciz "@(#)cmpd.s	1.4 (Berkeley) 6/1/90"
#endif /* SYSLIBC_SCCS and not lint */

#include "DEFS.h"

XENTRY(cmpd, R12|R11|R10|R9)
	movl	4(fp),r12
	movl	8(fp),r10
	movl	12(fp),r11
	movl	16(fp),r9
	tstl	r12
	jgeq	L16
	xorl2	$-2147483648,r12
	tstl	r10
	jeql	L17
	mnegl	r10,r10
	mcoml	r12,r12
	jbr	L16
L17:	mnegl	r12,r12
L16:	tstl	r11
	jgeq	L19
	xorl2	$-2147483648,r11
	tstl	r9
	jeql	L20
	mnegl	r9,r9
	mcoml	r11,r11
	jbr	L19
L20:	mnegl	r11,r11
L19:	cmpl	r12,r11
	jeql	L22
	cmpl	r12,r11
	jleq	L9999
	movl	$1,r0
	jbr	L9998
L9999:	mnegl	$1,r0
L9998:	ret
L22:	cmpl	r10,r9
	jeql	L23
	cmpl	r10,r9
	jlequ	L9997
	movl	$1,r0
	jbr	L9996
L9997:	mnegl	$1,r0
L9996:	ret
L23:	clrl	r0
	ret
