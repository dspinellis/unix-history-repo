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
 * from: $Header: _setjmp.s,v 1.1 91/07/06 16:45:53 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)_setjmp.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * C library -- _setjmp, _longjmp
 *
 *	_longjmp(a,v)
 * will generate a "return(v?v:1)" from
 * the last call to
 *	_setjmp(a)
 * by unwinding the call stack.
 * The previous signal state is NOT restored.
 */

#include "DEFS.h"

ENTRY(_setjmp)
	std	%sp, [%o0+0]	/* caller's stack pointer and return pc */
	st	%fp, [%o0+8]	/* store caller's frame pointer */
	retl
	 clr	%o0		! return 0

ENTRY(_longjmp)
	addcc	%o1, %g0, %g6	! compute v ? v : 1 in a global register
	be,a	0f
	 mov	1, %g6
0:
	mov	%o0, %g1	! save a in another global register
	ld	[%g1+8], %g7	/* get caller's frame */
1:
	cmp	%fp, %g7	! compare against desired frame
	bl,a	1b		! if below,
	 restore		!    pop frame and loop
	be,a	2f		! if there,
	 ldd	[%g1+0], %o2	!    fetch return %sp and pc, and get out

Lbotch:
	call	_longjmperror	! otherwise, went too far; bomb out
	nop
	unimp	0

2:
	cmp	%o2, %sp	! %sp must not decrease
	bge,a	3f
	 mov	%o2, %sp	! it is OK, put it in place
	b,a	Lbotch
3:
	jmp	%o3 + 8		! success, return %g6
	 mov	%g6, %o0
