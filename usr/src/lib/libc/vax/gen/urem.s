/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)urem.s	5.5 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * Unsigned modulus, PCC flavor.
 * urem() takes an ordinary dividend/divisor pair;
 * aurem() takes a pointer to a dividend and an ordinary divisor.
 */

#define	DIVIDEND	4(ap)
#define	DIVISOR		8(ap)

ASENTRY(urem,0)
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

ASENTRY(aurem,0)
	movl	DIVIDEND,r3
	movl	DIVISOR,r2
	jlss	La_easy		# big divisor: settle by comparison
	movl	(r3),r0
	jlss	La_hard		# big dividend: need extended division
	divl3	r2,r0,r1	# small divisor and dividend: signed modulus
	mull2	r2,r1
	subl2	r1,r0
	movl	r0,(r3)		# leave the value of the assignment in r0
	ret
La_hard:
	clrl	r1
	ediv	r2,r0,r1,r0
	movl	r0,(r3)
	ret
La_easy:
	subl3	r2,(r3),r0
	jcs	La_dividend	# if divisor is bigger, leave dividend alone
	movl	r0,(r3)		# if divisor goes in once, store difference
	ret
La_dividend:
	movl	(r3),r0
	ret
