#ifdef LIBC_SCCS
	.asciz	"@(#)modf.s	1.1 (Berkeley) %G%"
#endif LIBC_SCCS

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
	movl	12(fp),r3
	ldd	4(fp)
	tstd		# if (value < 0)
	jgeq	1f
	negd
	cvdl	r2	# ul = -value
	bvs	2f
	mnegl	r2,r0
	cvld	r0
	std	(r3)	# *iptr = -ul
	cvld	r2
	addd	4(fp)
	std	r0	# return (value + ul)
	ret
1:
	cvdl	r2	# ul = value
	bvs	2f
	cvld	r2
	std	(r3)	# *iptr = ul
	std	r0
	ldd	4(fp)
	subd	r0
	std	r0
	ret
2:			# integer overflow
	movl	4(fp),(r3)
	movl	8(fp),4(r3)
	clrl	r0
	clrl	r1
	ret
