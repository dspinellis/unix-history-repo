/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)umodsi3.s	6.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Unsigned modulus, GCC flavor.
 */

#include "DEFS.h"

#define	DIVIDEND	4(ap)
#define	DIVISOR		8(ap)

ENTRY(__umodsi3,0)
	movl	DIVISOR,r2
	jlss	Leasy		# big divisor: settle by comparison
	movl	DIVIDEND,r0
	jlss	Lhard		# big dividend: need extended division
	divl3	r2,r0,r1	# small divisor and dividend: signed modulus
	mull2	r2,r1
	subl2	r1,r0
	ret
Lhard:
	clrl	r1
	ediv	r2,r0,r1,r0
	ret
Leasy:
	subl3	r2,DIVIDEND,r0
	jcc	Ldifference	# if divisor goes in once, return difference
	movl	DIVIDEND,r0	# if divisor is bigger, return dividend
Ldifference:
	ret
