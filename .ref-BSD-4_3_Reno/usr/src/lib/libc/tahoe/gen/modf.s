/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)modf.s	1.5 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

/*
 * double modf (value, iptr)
 * double value, *iptr;
 *
 * Modf returns the fractional part of "value",
 * and stores the integer part indirectly through "iptr".
 *
 * This version uses floating point (look in ../fpe for
 * a much slower integer version).
 */

#include "DEFS.h"

ENTRY(modf, 0)
	ldd	4(fp)		# value
	cvdl	r2		# integerize
	bvs	1f		# did integer part overflow?
	cvld	r2		# integer part
	std	r0
	std	*12(fp)		# *iptr = r2
	ldd	4(fp)
	subd	r0		# value-(int)value
	std	r0		# return fraction
	ret
1:
	/*
	 * If the integer portion overflowed, mask out the fractional
	 * bits in the double word instead of cvdl-ing.
	 */
	ldd	4(fp)
	std	r0		# (r0,r1) = value
	shrl	$23,r0,r2	# extract sign,exponent of value
	andl2	$255,r2		# exponent
	subl2	$152,r2		# e-152
	/*
	 * If it overflowed then value>=2^31 and e>=160
	 * so we mask only r1 (low bits of fraction), not r0
	 */
	mnegl	$1,r3
	shrl	r2,r3,r3	# -1>>(e-152) is neg mask to clear fraction
	mcoml	r3,r3		# complement mask
	andl2	r3,r1		# mask off truly fractional bits from fraction
	ldd	r0		# now (r0,r1) = integerized value
	std	*12(fp)		# *iptr = integerized
	ldd	4(fp)
	subd	r0
	std	r0		# return fraction
	ret
