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
	.asciz "@(#)cvtld.s	1.2 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath/fp.h>
#include "DEFS.h"

XENTRY(cvtld, R2|R3|R4|R5)
 #
 #Some initializations:
 #
	clrl	r4		# r4 - negative flag.
	clrl	r2		# r2 - exponent.
	movl	4(fp),r0	# fetch operand.
	movl	r0,r5		# need another copy.
	jeql	retzero		# return zero.
	jgtr	positive
	mnegl	r0,r0
	jvs	retmin		# return minimum integer.
	incl	r4		# remember it was negative.
	movl	r0,r5		# remember the negated value.
 #
 #Compute exponent:
 #
positive:
	ffs	r0,r1
	incl 	r1
	addl2	r1,r2
	shrl	r1,r0,r0
	jneq	positive	# look for more set bits.
 #
 #we have the exponent in r2.
 #
	movl	r5,r0		# r0,r1 will hold the resulting f.p. number.
	clrl 	r1
 #
 #Shift the fraction part to its proper place:
 #
	subl3	r2,$HID_POS,r3
	jlss	shiftr		# if less then zero we have to shift right.
	shll	r3,r0,r0	# else we shift left.
	jmp	shifted
shiftr:
	mnegl	r3,r3
	shrq	r3,r0,r0
shifted:
	andl2	$CLEARHID,r0	# clear the hidden bit.
	shal	$EXPSHIFT,r2,r2	# shift the exponent to its proper place.
	orl2	$EXPSIGN,r2	# set the exponent sign bit(to bias it).
	orl2	r2,r0		# combine exponent & fraction.
	bbc	$0,r4,sign_ok	# do we  have to set the sign bit?
	orl2	$SIGNBIT,r0	# yes...
sign_ok:
	ret

retzero:
	clrl 	r0
	clrl	r1
	ret

retmin:
 	movl	$0xd0000000,r0
	clrl	r1
	ret


	
