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
 *	@(#)Kcvtdl.s	7.1 (Berkeley) 12/6/90
 */

#include "../math/fp.h"
#include "../math/Kfp.h"
#include "../tahoe/SYS.h"

 
	.text
	.globl	_Kcvtdl
	.globl	_Kcvtdu
_Kcvtdu:
_Kcvtdl:
	.word	R5|R4|R3|R2

 #
 #Some initializations:
 #
	clrl	r3		# r3 - negative flag.
	movl	4(fp),r0	# fetch operand.
	movl	8(fp),r1	
 #
 #get exponent
 #
	andl3	$EXPMASK,r0,r2	# r2 will hold the exponent.
	jeql	retzero	
	cmpl	$TWO_EXP,r2	# if exponent is less then -2,return zero.
	jgtr	retzero
	andl2	$0!EXPSIGN,r2	# turn off biased exponent sign
	shrl	$EXPSHIFT,r2,r2
 #
 #get fraction
 #
	bbc	$31,r0,positive	# if negative remember it.
	incl	r3
positive:
				# clear the non fraction parts.
	andl2	$(0!(EXPMASK | SIGNBIT)),r0
				# add the hidden bit.
	orl2	$(0!CLEARHID),r0
	subl2	$24,r2		# compute the shift.
	jgtr	shift_left
	mnegl	r2,r2
	shrl	r2,r0,r0	# shift right.
	jmp	shifted
shift_left:
	cmpl	r2,$7
	jgtr	overflow
go_on:	shlq	r2,r0,r0	# shift right.
shifted:
	bbc	$0,r3,done	# check for negative
	mnegl	r0,r0
done:	
	ret

retzero:
	clrl	r0
	ret
overflow:
	ret
