/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)modf.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * double modf (value, iptr)
 * double value, *iptr;
 *
 * Modf returns the fractional part of "value",
 * and stores the integer part indirectly through "iptr".
 */

#include "DEFS.h"

ENTRY(modf, 0)
	emodd	4(ap),$0,$0f1.0,r2,r0
	jvs	1f			# integer overflow
	cvtld	r2,*12(ap)
	ret
1:
	subd3	r0,4(ap),*12(ap)
	ret
