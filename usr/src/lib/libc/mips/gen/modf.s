/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)modf.s	5.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * double modf(val, iptr)
 *	double val, *iptr;
 * returns: xxx and n (in *iptr) where val == n.xxx
 */
LEAF(modf)
	li.d	$f6, 4503599627370496.0	# check for value out of range (2**52)
	abs.d	$f0, $f12
	c.lt.d	$f0, $f6
	mfc1	t0, $f13		# get the sign & exponent part
	bc1f	3f			# val is not less than maxint
	add.d	$f2, $f0, $f6		# logical shift right
	sub.d	$f2, $f2, $f6		# logical shift left
	c.le.d	$f2, $f0
	bc1t	1f
	li.d	$f6, 1.0		# adjust due to rounding
	sub.d	$f2, $f2, $f6
1:
	bge	t0, zero, 2f		# jump if val >= 0
	neg.d	$f2, $f0		# negate integer part if val < 0
2:
	s.d	$f2, 0(a2)		# save the integer part
	sub.d	$f0, $f12, $f2		# return the fractional part
	j	ra
3:
	mtc1	zero, $f0		# val was too big so
	mtc1	zero, $f1		#   return fraction of zero
	s.d	$f12, 0(a2)		#   and the original number.
	j	ra
END(modf)
