/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Kmodf.s	7.1 (Berkeley) %G%
 */

#include "../tahoe/SYS.h"
#include "../math/fp.h"

/* 
 *  float Kmodf (value, iptr, hfs)
 *  float value, *iptr;
 *  int hfs;
 * 
 *  Modf returns the fractional part of "value",
 *  and stores the integer part indirectly through "iptr".
 */ 
	.text
ENTRY(Kmodf, R8|R7|R6|R5|R4|R3|R2)
 /*
 * Some initializations:
 */
	clrl	r3
	movl	4(fp),r0	/* fetch operand to r0. */
	movl	8(fp),r1
	movl	12(fp),r6	/* fetch addr of int to r6. */
 /*
  * get exponent
  */
	andl3	$EXPMASK,r0,r2	/* r2 will hold the exponent. */
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
	jgtr	in_r1		/* some bytes in r1	*/
	jeql	onlyallr0	/* all r0 must be cleared. */
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
	clrl	r0
	subl3	r2,$HID_POS,r7
	movl	$0x7fffffff,r8
1:
	andl2	r8,r1
	shar	$1,r8,r8
	orl2	$0x80000000,r8
	aoblss	$0,r7,1b
1:
	andl2	r8,r5
	shar	$1,r8,r8
	cmpl	$0xffffffff,r8
	jneq	1b
norm:
	addl2	$BIAS,r2	/* fnorm expects it biased. */
	pushl	16(fp)		/* hfs */
	callf	$8,_Kfnorm	/* normelize fraction part. */
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
				/* the fraction is ready in r0. */
	clrl	(r6)		/* zero the int part. */
	clrl	4(r6)
	ret

ret_zero:
	clrl	(r6)
 	clrl	4(r6)	
	clrl	r0
	clrl	r1
	ret
