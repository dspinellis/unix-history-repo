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
	.asciz "@(#)modf.s	8.1 (Berkeley) 6/4/93"
#endif /* SYSLIBC_SCCS and not lint */

/* 
 *  double modf (value, iptr)
 *  double value, *iptr;
 * 
 *  Modf returns the fractional part of "value",
 *  and stores the integer part indirectly through "iptr".
 */ 
#include <tahoemath/fp.h>
#include "DEFS.h"
 
ENTRY(modf, R2|R3|R4|R5)
 /*
 * Some initializations:
 */
	ldd	4(fp)		/* load accumulator, for converison	*/
	cvdl	r2		/*  to an integer.			*/
	bvs	gsb		/* iff too big grunt it out		*/
	cvld	r2		/* float the integer part		*/
	std	r2
	ldd	4(fp)		/* isolate the fraction			*/
	subd	r2
	std	r0
	movl	12(fp),r6	/* get int return address */
	movl	r2,(r6)
	movl	r3,4(r6)
	ret
gsb:	clrl	r3
	movl	4(fp),r0	/* fetch operand to r0,r1. */
	movl	8(fp),r1	
	movl	12(fp),r6	/* fetch addr of int to r6. */
 /*
 * get exponent
 */
	andl3	$EXPMASK,r0,r2	/* r2 will hold the exponent. */
	jeql	is_reserved	/* check for reserved operand.  */
	shrl	$EXPSHIFT,r2,r2
	subl2	$BIAS,r2	/* unbias it.  */
	jleq	allfrac		/* it's int part is  zero. */
	cmpl	r2,$56
	jgeq	allint		/* it's fraction part is zero. */
 /*
 * get fraction
 */
	movl	r0,r4		/* remember the original number. */
	movl	r1,r5
	bbc	$31,r0,positive	/* if negative remember it. */
	incl	r3
positive:
				/* clear the non fraction parts. */
	andl2	$(0!(EXPMASK | SIGNBIT)),r0
				/* add the hidden bit. */
	orl2	$(0!CLEARHID),r0

	cmpl	r2,$HID_POS	/* see if there are bits to clear only in r0 */
				/* or r1 has to be taken care of. */
				/* ( for fraction calculation) */

	jgtr	in_r1		/* some bytes in r1. */

	jeql	onlyallr0	/* all r0 must be cleared,r1 unchanged. */

				/* only r0 must be canged. */
	mnegl	r2,r7		/* r7 - loop counter. */
	movl	$CLEARHID,r8	/* first bit to clear. */
1:
	andl2	r8,r0		/* clear int. bits from fraction part. */
	shar	$1,r8,r8
	aoblss	$0,r7,1b
1:
	andl2	r8,r4		/* clear frac bits for int calculation: */
	shar	$1,r8,r8
	cmpl	$0xffffffff,r8
	jneq	1b
	clrl	r5
	jmp	norm

onlyallr0:
	clrl	r0
	clrl	r5
	jmp 	norm

in_r1:
	clrl	r0		/* clear int part for frac. calculation. */
	subl3	r2,$HID_POS,r7
	movl	$0x7fffffff,r8
1:
	andl2	r8,r1
	shar	$1,r8,r8
	orl2	$0x80000000,r8		/* force the 'sign' bit to be on. */
	aoblss	$0,r7,1b
1:
	andl2	r8,r5
	shar	$1,r8,r8
	cmpl	$0xffffffff,r8
	jneq	1b

norm:
	addl2	$BIAS,r2	/* fnorm expects it biased. */
	callf	$4,fnorm	/* normelize fraction part. */
	cmpl	$0,r0
	jeql	1f
	bbc	$0,r3,1f
	orl2	$0x80000000,r0
1:
	movl	r4,(r6)		/* put int part in place. */
	movl	r5,4(r6)
	ret
 
allint:
	movl	r0,(r6)		/* copy the argument to the int addr. */
	movl	r1,4(r6)
	clrl	r0		/* clear the fraction part. */
	clrl	r1
	ret

allfrac:
				/* the fraction is ready in r0,r1. */
	clrl	(r6)		/* zero the int part. */
	clrl	4(r6)
	ret

ret_zero:
	clrl	(r6)
	clrl	4(r6)
	clrl	r0
	clrl	r1
	ret

is_reserved:
	bbc	$31,r0,ret_zero
	callf 	$4,fpresop
	ret
