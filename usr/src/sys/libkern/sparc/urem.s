
/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
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
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS `AS IS' AND
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
 * from: $Header: divrem.m4,v 1.4 92/06/25 13:23:57 torek Exp $
 */

/*
 * Division and remainder, from Appendix E of the Sparc Version 8
 * Architecture Manual, with fixes from Gordon Irlam.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)divrem.m4	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Input: dividend and divisor in %o0 and %o1 respectively.
 *
 * m4 parameters:
 *  .urem	name of function to generate
 *  rem		rem=div => %o0 / %o1; rem=rem => %o0 % %o1
 *  false		false=true => signed; false=false => unsigned
 *
 * Algorithm parameters:
 *  N		how many bits per iteration we try to get (4)
 *  WORDSIZE	total number of bits (32)
 *
 * Derived constants:
 *  TWOSUPN	2^N, for label generation (m4 exponentiation currently broken)
 *  TOPBITS	number of bits in the top decade of a number
 *
 * Important variables:
 *  Q		the partial quotient under development (initially 0)
 *  R		the remainder so far, initially the dividend
 *  ITER	number of main division loop iterations required;
 *		equal to ceil(log2(quotient) / N).  Note that this
 *		is the log base (2^N) of the quotient.
 *  V		the current comparand, initially divisor*2^(ITER*N-1)
 *
 * Cost:
 *  Current estimate for non-large dividend is
 *	ceil(log2(quotient) / N) * (10 + 7N/2) + C
 *  A large dividend is one greater than 2^(31-TOPBITS) and takes a
 *  different path, as the upper bits of the quotient must be developed
 *  one bit at a time.
 */













/* m4 reminder: d => if a is b, then c, else d */




/*
 * This is the recursive definition for developing quotient digits.
 *
 * Parameters:
 *  $1	the current depth, 1 <= $1 <= 4
 *  $2	the current accumulation of quotient bits
 *  4	max depth
 *
 * We add a new bit to $2 and either recurse or insert the bits in
 * the quotient.  %o3, %o2, and %o5 are inputs and outputs as defined above;
 * the condition codes are expected to reflect the input %o3, and are
 * modified to reflect the output %o3.
 */


#include "DEFS.h"
#include <machine/trap.h>

FUNC(.urem)

	! Ready to divide.  Compute size of quotient; scale comparand.
	orcc	%o1, %g0, %o5
	bnz	1f
	mov	%o0, %o3

		! Divide by zero trap.  If it returns, return 0 (about as
		! wrong as possible, but that is what SunOS does...).
		t	ST_DIV0
		retl
		clr	%o0

1:
	cmp	%o3, %o5			! if %o1 exceeds %o0, done
	blu	Lgot_result		! (and algorithm fails otherwise)
	clr	%o2
	sethi	%hi(1 << (32 - 4 - 1)), %g1
	cmp	%o3, %g1
	blu	Lnot_really_big
	clr	%o4

	! Here the dividend is >= 2^(31-N) or so.  We must be careful here,
	! as our usual N-at-a-shot divide step will cause overflow and havoc.
	! The number of bits in the result here is N*ITER+SC, where SC <= N.
	! Compute ITER in an unorthodox manner: know we need to shift V into
	! the top decade: so do not even bother to compare to R.
	1:
		cmp	%o5, %g1
		bgeu	3f
		mov	1, %g7
		sll	%o5, 4, %o5
		b	1b
		inc	%o4

	! Now compute %g7.
	2:	addcc	%o5, %o5, %o5
		bcc	Lnot_too_big
		inc	%g7

		! We get here if the %o1 overflowed while shifting.
		! This means that %o3 has the high-order bit set.
		! Restore %o5 and subtract from %o3.
		sll	%g1, 4, %g1	! high order bit
		srl	%o5, 1, %o5		! rest of %o5
		add	%o5, %g1, %o5
		b	Ldo_single_div
		dec	%g7

	Lnot_too_big:
	3:	cmp	%o5, %o3
		blu	2b
		nop
		be	Ldo_single_div
		nop
	/* NB: these are commented out in the V8-Sparc manual as well */
	/* (I do not understand this) */
	! %o5 > %o3: went too far: back up 1 step
	!	srl	%o5, 1, %o5
	!	dec	%g7
	! do single-bit divide steps
	!
	! We have to be careful here.  We know that %o3 >= %o5, so we can do the
	! first divide step without thinking.  BUT, the others are conditional,
	! and are only done if %o3 >= 0.  Because both %o3 and %o5 may have the high-
	! order bit set in the first step, just falling into the regular
	! division loop will mess up the first time around.
	! So we unroll slightly...
	Ldo_single_div:
		deccc	%g7
		bl	Lend_regular_divide
		nop
		sub	%o3, %o5, %o3
		mov	1, %o2
		b	Lend_single_divloop
		nop
	Lsingle_divloop:
		sll	%o2, 1, %o2
		bl	1f
		srl	%o5, 1, %o5
		! %o3 >= 0
		sub	%o3, %o5, %o3
		b	2f
		inc	%o2
	1:	! %o3 < 0
		add	%o3, %o5, %o3
		dec	%o2
	2:
	Lend_single_divloop:
		deccc	%g7
		bge	Lsingle_divloop
		tst	%o3
		b,a	Lend_regular_divide

Lnot_really_big:
1:
	sll	%o5, 4, %o5
	cmp	%o5, %o3
	bleu	1b
	inccc	%o4
	be	Lgot_result
	dec	%o4

	tst	%o3	! set up for initial iteration
Ldivloop:
	sll	%o2, 4, %o2
		! depth 1, accumulated bits 0
	bl	L.1.16
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 2, accumulated bits 1
	bl	L.2.17
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 3, accumulated bits 3
	bl	L.3.19
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 4, accumulated bits 7
	bl	L.4.23
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (7*2+1), %o2
	
L.4.23:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (7*2-1), %o2
	
	
L.3.19:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 4, accumulated bits 5
	bl	L.4.21
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (5*2+1), %o2
	
L.4.21:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (5*2-1), %o2
	
	
	
L.2.17:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 3, accumulated bits 1
	bl	L.3.17
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 4, accumulated bits 3
	bl	L.4.19
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (3*2+1), %o2
	
L.4.19:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (3*2-1), %o2
	
	
L.3.17:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 4, accumulated bits 1
	bl	L.4.17
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (1*2+1), %o2
	
L.4.17:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (1*2-1), %o2
	
	
	
	
L.1.16:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 2, accumulated bits -1
	bl	L.2.15
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 3, accumulated bits -1
	bl	L.3.15
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 4, accumulated bits -1
	bl	L.4.15
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-1*2+1), %o2
	
L.4.15:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-1*2-1), %o2
	
	
L.3.15:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 4, accumulated bits -3
	bl	L.4.13
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-3*2+1), %o2
	
L.4.13:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-3*2-1), %o2
	
	
	
L.2.15:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 3, accumulated bits -3
	bl	L.3.13
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
			! depth 4, accumulated bits -5
	bl	L.4.11
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-5*2+1), %o2
	
L.4.11:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-5*2-1), %o2
	
	
L.3.13:
	! remainder is negative
	addcc	%o3,%o5,%o3
			! depth 4, accumulated bits -7
	bl	L.4.9
	srl	%o5,1,%o5
	! remainder is positive
	subcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-7*2+1), %o2
	
L.4.9:
	! remainder is negative
	addcc	%o3,%o5,%o3
		b	9f
		add	%o2, (-7*2-1), %o2
	
	
	
	
	9:
Lend_regular_divide:
	deccc	%o4
	bge	Ldivloop
	tst	%o3
	bl,a	Lgot_result
	! non-restoring fixup here (one instruction only!)
	add	%o3, %o1, %o3


Lgot_result:

	retl
	mov %o3, %o0
