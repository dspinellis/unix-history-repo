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
 * from: $Header: modf.s,v 1.3 92/06/20 00:00:54 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)modf.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"
#include <machine/fsr.h>

/*
 * double modf(double val, double *iptr)
 *
 * Returns the fractional part of `val', storing the integer part of
 * `val' in *iptr.  Both *iptr and the return value have the same sign
 * as `val'.
 *
 * Method:
 *
 * We use the fpu's normalization hardware to compute the integer portion
 * of the double precision argument.  Sun IEEE double precision numbers
 * have 52 bits of mantissa, 11 bits of exponent, and one bit of sign,
 * with the sign occupying bit 31 of word 0, and the exponent bits 30:20
 * of word 0.  Thus, values >= 2^52 are by definition integers.
 *
 * If we take a value that is in the range [+0..2^52) and add 2^52, all
 * of the fractional bits fall out and all of the integer bits are summed
 * with 2^52.  If we then subtract 2^52, we get those integer bits back.
 * This must be done with rounding set to `towards 0' or `towards -inf'.
 * `Toward -inf' fails when the value is 0 (we get -0 back)....
 *
 * Note that this method will work anywhere, but is machine dependent in
 * various aspects.
 *
 * Stack usage:
 *	4@[%fp - 4]	saved %fsr
 *	4@[%fp - 8]	new %fsr with rounding set to `towards 0'
 *	8@[%fp - 16]	space for moving between %i and %f registers
 * Register usage:
 *	%i0%i1		double val;
 *	%l0		scratch
 *	%l1		sign bit (0x80000000)
 *	%i2		double *iptr;
 *	%f2:f3		`magic number' 2^52, in fpu registers
 *	%f4:f5		double v, in fpu registers
 */

	.align	8
Lmagic:
	.word	0x43300000	! sign = 0, exponent = 52 + 1023, mantissa = 0
	.word	0		! (i.e., .double 0r4503599627370496e+00)

L0:
	.word	0		! 0.0
	.word	0

ENTRY(modf)
	save	%sp, -64-16, %sp

	/*
	 * First, compute v = abs(val) by clearing sign bit,
	 * and then set up the fpu registers.  This would be
	 * much easier if we could do alu operations on fpu registers!
	 */
	sethi	0x80000000, %l1		! sign bit
	andn	%i0, %l1, %l0
	st	%l0, [%fp - 16]
	sethi	%hi(Lmagic), %l0
	ldd	[%l0 + %lo(Lmagic)], %f2
	st	%i1, [%fp - 12]
	ldd	[%fp - 16], %f4		! %f4:f5 = v

	/*
	 * Is %f4:f5 >= %f2:f3 ?  If so, it is all integer bits.
	 * It is probably less, though.
	 */
	fcmped	%f4, %f2
	nop				! fpop2 delay
	fbuge	Lbig			! if >= (or unordered), go out
	nop

	/*
	 * v < 2^52, so add 2^52, then subtract 2^52, but do it all
	 * with rounding set towards zero.  We leave any enabled
	 * traps enabled, but change the rounding mode.  This might
	 * not be so good.  Oh well....
	 */
	st	%fsr, [%fp - 4]		! %l5 = current FSR mode
	set	FSR_RD, %l3		! %l3 = rounding direction mask
	ld	[%fp - 4], %l5
	set	FSR_RD_RZ << FSR_RD_SHIFT, %l4
	andn	%l5, %l3, %l6
	or	%l6, %l4, %l6		! round towards zero, please
	and	%l5, %l3, %l5		! save original rounding mode
	st	%l6, [%fp - 8]
	ld	[%fp - 8], %fsr

	faddd	%f4, %f2, %f4		! %f4:f5 += 2^52
	fsubd	%f4, %f2, %f4		! %f4:f5 -= 2^52

	/*
	 * Restore %fsr, but leave exceptions accrued.
	 */
	st	%fsr, [%fp - 4]
	ld	[%fp - 4], %l6
	andn	%l6, %l3, %l6		! %l6 = %fsr & ~FSR_RD;
	or	%l5, %l6, %l5		! %l5 |= %l6;
	st	%l5, [%fp - 4]
	ld	[%fp - 4], %fsr		! restore %fsr, leaving accrued stuff

	/*
	 * Now insert the original sign in %f4:f5.
	 * This is a lot of work, so it is conditional here.
	 */
	btst	%l1, %i0
	be	1f
	nop
	st	%f4, [%fp - 16]
	ld	[%fp - 16], %g1
	or	%l1, %g1, %g1
	st	%g1, [%fp - 16]
	ld	[%fp - 16], %f4
1:

	/*
	 * The value in %f4:f5 is now the integer portion of the original
	 * argument.  We need to store this in *ival (%i2), subtract it
	 * from the original value argument (%i0:i1), and return the result.
	 */
	std	%f4, [%i2]		! *ival = %f4:f5;
	std	%i0, [%fp - 16]
	ldd	[%fp - 16], %f0		! %f0:f1 = val;
	fsubd	%f0, %f4, %f0		! %f0:f1 -= %f4:f5;
	ret
	restore

Lbig:
	/*
	 * We get here if the original comparison of %f4:f5 (v) to
	 * %f2:f3 (2^52) came out `greater or unordered'.  In this
	 * case the integer part is the original value, and the
	 * fractional part is 0.
	 */
	sethi	%hi(L0), %l0
	std	%f0, [%i2]		! *ival = val;
	ldd	[%l0 + %lo(L0)], %f0	! return 0.0;
	ret
	restore
