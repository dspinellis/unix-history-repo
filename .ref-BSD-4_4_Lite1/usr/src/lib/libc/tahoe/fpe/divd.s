/*
 * Copyright (c) 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)divd.s	8.1 (Berkeley) 6/4/93"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath/fp.h>
#include "DEFS.h"

#define	HIDDEN	23	/* here we count from 0 not from 1 as in fp.h */

XENTRY(divd, R2|R3|R4|R5|R6|R7|R8|R9)
	clrl	r3		# r3 - sign: 0 for positive,1 for negative.
	movl	4(fp),r0
	jgeq	1f
	movl	$1,r3
1:	movl	12(fp),r2
	jgeq	2f
	bbc	$0,r3,1f	# seconed operand is negative.
	clrl	r3		# if first was negative, make result positive.
	jmp	2f
1:	movl	$1,r3		# if first was positive, make result negative.
2:	andl2	$EXPMASK,r0	# compute first 'pure'exponent.
	jeql	is_res1
	shrl	$EXPSHIFT,r0,r0
	subl2	$BIAS,r0	
	andl2	$EXPMASK,r2	# compute seconed 'pure'exponent.
	jeql	is_res2
	shrl	$EXPSHIFT,r2,r2
	subl2	$BIAS,r2
	subl3	r2,r0,r2	# subtruct the exponents.
	addl2	$BIAS,r2
	jleq	underf
				# normalization can make the exp. smaller.
 #
 #	We have the sign in r3,the exponent in r2,now is the time to
 # 	perform the division...
 #
	# fetch dividend. (r4,r5)
	andl3	$(0!(EXPMASK | SIGNBIT)),4(fp),r4
	orl2	$(0!CLEARHID),r4
	movl	8(fp),r5
 
	# fetch divisor : (r6,r7)
	andl3	$(0!(EXPMASK | SIGNBIT)),12(fp),r6
	orl2	$(0!CLEARHID),r6
	movl	16(fp),r7

	movl	$0,r0		# init r0,r1 to be zeros
	movl	$0,r1
	movl	$(0!CLEARHID),r8# r8 first bit to set (if).
	shll	$1,r8,r8	# to have one more bit,because we might
				# have to shift left to normelize.
	movl	$0,r9

2:
	subl2	r7,r5
	sbwc	r6,r4
	jgeq	1f
	addl2	r7,r5
	adwc	r6,r4
	shlq	$1,r4,r4
	shrq	$1,r8,r8
	jeql	over
	jmp	2b
1:	
	orl2	r8,r0
	orl2	r9,r1
	shlq	$1,r4,r4
	shrq	$1,r8,r8
	jneq	2b
	
over:
	callf	$4,fnorm
sign:
1:	bbc	$0,r3,done
	orl2	$SIGNBIT,r0
done:	ret

is_res1:
	bbc 	$31,4(fp),retz
	callf	$4,fpresop
	ret
is_res2:
	bbc 	$31,12(fp),z_div
	callf	$4,fpresop
	ret
retz:
	  clrl	r0
	  clrl	r1
	  ret
underf:
	callf	$4,fpunder
	ret
z_div:
	callf	$4,fpzdiv
	ret
