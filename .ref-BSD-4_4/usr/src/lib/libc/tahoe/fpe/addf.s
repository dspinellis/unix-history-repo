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
	.asciz "@(#)addf.s	8.1 (Berkeley) 6/4/93"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath//fp.h>
#include "DEFS.h"

XENTRY(addf, R2|R3|R4|R5|R6|R7|R8|R9|R10)
/*
 * see which operand has a greater exponent
 * The greater one will be fetched into r0,r2,r3.
 * r0- 'pure' fraction, r2 - exponent, r3 - sign).
 * The smaller operand will be fetched into r4,r6,r7.
 */
	clrl	r1
	andl3	$EXPMASK,4(fp),r0
	andl3	$EXPMASK,12(fp),r1
	cmpl	r0,r1
	jgtr	first_greater

	movl	12(fp),r0	# bigger operand to r0

	movl	4(fp),r4	# smaller operand to r4
	jmp	expo

first_greater:
	movl	4(fp),r0	# bigger operand to r0

	movl	12(fp),r4	# smaller operand to r4


/*
 *compute exponents:
 */
expo:
	andl3	$EXPMASK,r0,r2	# r2 will hold the exponent of greater operand.
	jeql	is_res1		# check for reserved operand. 
	shrl	$EXPSHIFT,r2,r2


	andl3	$EXPMASK,r4,r6	# r6 will hold the exponent of smaller operand.
	jeql	is_res2		# check for reserved operand. 
	shrl	$EXPSHIFT,r6,r6
/*
 *compare the exponents:
 */
	subl3	r6,r2,r8
	jeql	signs
	cmpl	r8,$MAX_EXP_DIF
	jlss	signs
	ret			# return the bigger number.
 
/*
 *remember the signs:
 */
signs:
	clrl	r3
	bbc	$31,r0,sign2	# if negative remember it.(R3=1)
	incl	r3
sign2:
	clrl	r7
	bbc	$31,r4,frac	# if negative remember it.(R7=1)
	incl	r7
/*
 *compute 'pure' fraction:
 */
frac:
				# clear the non fraction parts.
	andl2	$(0!(EXPMASK | SIGNBIT)),r0
				# add the hidden bit.
	orl2	$(0!CLEARHID),r0
				# clear the non fraction parts.
	andl2	$(0!(EXPMASK | SIGNBIT)),r4
				# add the hidden bit.
	orl2	$(0!CLEARHID),r4

/*
 *shift the smaller operand:
 */
	shar	r8,r4,r4
eql_exps:
	cmpl 	r3,r7
	jeql	add
	bbc	$0,r3,negr4
/*
 *negate r0:
 */
	clrl	r3
	mnegl	r0,r0

/*
 *add the fractions:
 */
add:
	clrl	r10
	addl2	r4,r0
	jgeq	norm
	incl	r10
/*
 *negate the pair r0,r1:
 */
	mnegl	r0,r0
norm:	callf	$4,sfnorm
 
/*
 *add the sign bit
 */
	bbs	$0,r10,negative
	bbs	$0,r3,negative	# the bigger operand was negative.
	ret
negative:
	orl2	$SIGNBIT,r0
	ret
 
 
/*
 *negate r4:
 */
negr4:
	mnegl	r4,r4
	jmp	add
 
 
is_res1:
	bbs	$31,r0,res_op
	movl	r4,r0		# return the  smaller operand.
	ret

is_res2:
	bbs	$31,r4,res_op
	ret			# we allready have the 'result' in r0,r1.

res_op:
	callf	$4,sfpresop
	ret
