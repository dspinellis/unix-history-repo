/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: fixunsdfsi.s,v 1.3 91/10/08 00:03:15 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fixunsdfsi.s	8.1 (Berkeley) %G%"
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
