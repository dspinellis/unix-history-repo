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
	.asciz "@(#)sfnorm.s	1.3 (Berkeley) 6/1/90"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath/fp.h>
#include "DEFS.h"

XENTRY(sfnorm, R2|R3|R4|R5|R6)
	clrl	r1
	movl	r0,r4		/* copy to temporary. */
	jeql	retzero
	clrl	r3		/* r3 - pos of m.s.b */
inr00:	ffs	r4,r6
	incl	r6
	addl2	r6,r3
	shrl	r6,r4,r4
	jneq	inr00

cmpshift:
				/* compute the shift (r4). */
	subl3	r3,$HID_POS,r4
	jlss	shiftr		/* if less then zero we shift right. */
	shll	r4,r0,r0	/* else we shift left. */
	subl2	r4,r2		/* uodate exponent. */
	jleq	underflow	/* if less then 0 (biased) it is underflow. */
	jmp	combine		/* go to combine exponent and fraction. */
shiftr:
	mnegl	r4,r4
	shrl	r4,r0,r0	/* shift right. */
	addl2	r4,r2		/* update exponent */
	cmpl	r2,$256
	jgeq	overflow	/* check for overflow. */
combine:
	andl2	$CLEARHID,r0	/* clear the hidden bit. */
	shal	$EXPSHIFT,r2,r2	/* shift the exponent to its proper place. */
	orl2	r2,r0
	ret

underflow:
	callf	$4,sfpunder
	ret

overflow:
	callf 	$4,sfpover
	ret
retzero:
	clrl	r0
	ret
