/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)sfnorm.s	1.2 (Berkeley) %G%"
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
