/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)Kmuld.s	7.1 (Berkeley) 12/6/90
 */

#include "../math/fp.h"
#include "../math/Kfp.h"
#include "../tahoe/SYS.h"

#define	HIDDEN	23	/* here we count from 0 not from 1 as in fp.h */

/*
 * _Kmuld(acc_most,acc_least,op_most,op_least,hfs)
 */
	.text
ENTRY(Kmuld, R9|R8|R7|R6|R5|R4|R3|R2)
	clrl	r3		/* r3 - sign: 0 for positive,1 for negative. */
	movl	4(fp),r0
	jgeq	1f
	movl	$1,r3
1:	movl	12(fp),r2
	jgeq	2f
	bbc	$0,r3,1f	/* seconed operand is negative. */
	clrl	r3		/* if first was neg, make result pos */
	jmp	2f
1:	movl	$1,r3		/* if first was pos, make result neg */
2:	andl2	$EXPMASK,r0	/* compute first 'pure'exponent. */
	jeql	retzero
	shrl	$EXPSHIFT,r0,r0
	subl2	$BIASP1,r0	
	andl2	$EXPMASK,r2	/* compute seconed 'pure'exponent. */
	jeql	retzero
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
	/* fetch first fraction: (r0,r1) */
	andl3	$(0!(EXPMASK | SIGNBIT)),4(fp),r0
	orl2	$(0!CLEARHID),r0
	movl	8(fp),r1
	shlq	$7,r0,r0	/* leave the sign bit cleared. */
 
	/* fetch seconed fraction: (r4,r5) */
	andl3	$(0!(EXPMASK | SIGNBIT)),12(fp),r4
	orl2	$(0!CLEARHID),r4
	movl	16(fp),r5
	shlq	$7,r4,r4	/* leave the sign bit cleared. */

	/* in the following lp1 stands for least significant part of operand 1,
	*		   lp2 for least significant part of operand 2,
	*		   mp1 for most significant part of operand 1,
	*		   mp2 for most significant part of operand 2.
	*/
 
	clrl 	r6
	shrl	$1,r1,r1	/* clear the sign bit of the lp1. */
	jeql	1f
	emul	r1,r4,$0,r6	/* r6,r7 <-- lp1*mp2 */
	shlq	$1,r6,r6	/* to compensate for the shift we did to clear the sign bit. */
1:	shrl	$1,r5,r5	/* clear the sign bit of the lp2. */
	jeql	1f
	emul	r0,r5,$0,r8	/* r8,r9 <-- mp1*lp2 */
	shlq	$1,r8,r8
	addl2	r9,r7		/* r6,r7 <-- the sum of the products. */
	adwc	r8,r6
1:	emul	r0,r4,$0,r0	/* r0,r1 <-- mp1*mp2  */
	addl2	r6,r1		/* add the most sig. part of the sum. */
	adwc	$0,r0
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
	shrq	r5,r0,r0
	bbc	$0,r1,shiftmore
	incl	r1		/* rounding. */
shiftmore:
	shrq	$1,r0,r0
comb:
	andl2	$CLEARHID,r0
	shll	$EXPSHIFT,r2,r4
	orl2	r4,r0
	cmpl	r2,$256
	jlss	1f
	orl2	$HFS_OVF,*20(fp)	
sign:
1:	bbc	$0,r3,done
	orl2	$SIGNBIT,r0
done:	ret

retzero:
	clrl	r0
	clrl	r1
	ret
overflow:
	orl2	$HFS_OVF,*20(fp)
	ret
underflow:
	orl2	$HFS_UNDF,*20(fp)
	ret
