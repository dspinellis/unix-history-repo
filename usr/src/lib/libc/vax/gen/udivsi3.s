/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Donn Seeley at UUNET Technologies, Inc.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)udivsi3.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Unsigned division, GCC flavor.
 */

#include "DEFS.h"

#define	DIVIDEND	4(ap)
#define	DIVISOR		8(ap)

ENTRY(__udivsi3,0)
	movl	DIVISOR,r2
	jlss	Leasy		# big divisor: settle by comparison
	movl	DIVIDEND,r0
	jlss	Lhard		# big dividend: extended division
	divl2	r2,r0		# small divisor and dividend: signed division
	ret
Lhard:
	clrl	r1
	ediv	r2,r0,r0,r1
	ret
Leasy:
	cmpl	DIVIDEND,r2
	jgequ	Lone		# if dividend is as big or bigger, return 1
	clrl	r0		# else return 0
	ret
Lone:
	movl	$1,r0
	ret
