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
 * from: $Header: fixunsdfsi.s,v 1.3 91/10/08 00:03:15 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fixunsdfsi.s	8.1 (Berkeley) 6/4/93"
#endif /* LIBC_SCCS and not lint */

/*
 * Convert double to unsigned integer (for gcc).
 *
 * I have made the output for NaN agree with the Sun compiler, not
 * that it really matters, by using `fbul,a'.
 */

#include "DEFS.h"

	.align	8
Lbig:
	.word	0x41e00000		! .double 0r2147483648.0e+00
	.word	0			! (who me, not trust the assembler?)

ENTRY(__fixunsdfsi)
	sub	%sp, 8, %sp
	std	%o0, [%sp + 64]		! get argument into fpu reg
	ldd	[%sp + 64], %f0
	sethi	%hi(Lbig), %g1
	ldd	[%g1 + %lo(Lbig)], %f2
	fcmped	%f0, %f2		! d < 2^31, or NaN, or -Inf?
	nop				! (fpop2 delay)
	fbul,a	1f			! if so, use fdtoi to convert to int
	fdtoi	%f0, %f0		!        (this includes negatives!)

	! d does not fit in an int, so subtract 2^31, convert,
	! and add 2^31 again (sigh).  Just hope the intermediate
	! fits (if not, the result is undefined anyway).

	fsubd	%f0, %f2, %f0		! d -= 2^31
	fdtoi	%f0, %f0		! convert to int
	st	%f0, [%sp + 64]		! move into return reg
	ld	[%sp + 64], %o0
	sethi	%hi(0x80000000), %o1
	add	%o0, %o1, %o0		! add 2^31
	retl
	add	%sp, 8, %sp

1:
	st	%f0, [%sp + 64]		! return result
	ld	[%sp + 64], %o0
	retl
	add	%sp, 8, %sp
