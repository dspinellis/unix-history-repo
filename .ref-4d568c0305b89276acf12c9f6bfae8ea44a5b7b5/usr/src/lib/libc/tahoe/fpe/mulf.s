/*
 * Copyright (c) 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)mulf.s	8.1 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath/fp.h>
#include "DEFS.h"

#define	HIDDEN	23	/* here we count from 0 not from 1 as in fp.h */

XENTRY(mulf, R2|R3|R4|R5|R6|R7|R8|R9)
	clrl	r3		/* r3 - sign: 0 for positive,1 for negative. */
	movl	4(fp),r0
	jgeq	1f
	movl	$1,r3
1:	movl	12(fp),r2
	jgeq	2f
	bbc	$0,r3,1f	/* seconed operand is negative. */
	clrl	r3		/* if first was negative, make result positive. */
	jmp	2f
1:	movl	$1,r3		/* if first was positive, make result negative. */
2:	andl2	$EXPMASK,r0	/* compute first 'pure'exponent. */
	jeql	is_res1
	shrl	$EXPSHIFT,r0,r0
	subl2	$BIASP1,r0	
	andl2	$EXPMASK,r2	/* compute seconed 'pure'exponent. */
	jeql	is_res2
	shrl	$EXPSHIFT,r2,r2
	subl2	$BIASP1,r2
	addl2	r0,r2		/* add the exponents. */
	addl2	$(BIASP1+2),r2
	jleq	underflow
	cmpl	r2,$258		/* normalization can make the exp. smaller. */
	jgeq	overflow
 /*
 *	We have the sign in r3,the exponent in r2,now is the time to
 * 	perform the multiplication...
 */
	/* fetch first fraction: (r0) */
	andl3	$(0!(EXPMASK | SIGNBIT)),4(fp),r0
	orl2	$(0!CLEARHID),r0
	shll	$7,r0,r0	/* leave the sign bit cleared. */
 
	/* fetch seconed fraction: (r4) */
	andl3	$(0!(EXPMASK | SIGNBIT)),12(fp),r4
	orl2	$(0!CLEARHID),r4
	shll	$7,r4,r4	/* leave the sign bit cleared. */

 
	emul	r4,r0,$0,r0
	movl	r0,r4		/* to see how much we realy need to shift. */
	movl	$6,r5		/* r5 - shift counter. */
	shrl	$7,r4,r4	/* dummy shift. */
1:	bbs	$HIDDEN,r4,realshift
	shll	$1,r4,r4
	decl	r2		/* update exponent. */
	jeql	underflow
	decl	r5		/* update shift counter. */
	jmp	1b
realshift:
	shrl	r5,r0,r0
	bbc	$0,r1,shiftmore
	incl	r1		/* rounding. */
shiftmore:
	shrl	$1,r0,r0
comb:
	andl2	$CLEARHID,r0
	shll	$EXPSHIFT,r2,r4
	orl2	r4,r0
	cmpl	r2,$256
	jlss	1f
	callf	$4,sfpover
sign:
1:	bbc	$0,r3,done
	orl2	$SIGNBIT,r0
done:	ret



is_res1:
	bbc 	$31,4(fp),retzero
	callf	$4,sfpresop
	ret
is_res2:
	bbc 	$31,12(fp),retzero
	callf	$4,sfpresop
	ret
  retzero:
	  clrl	r0
	  ret
  overflow:
	callf	$4,sfpover
	jmp	sign
  underflow:
	callf	$4,sfpunder
	ret


	
