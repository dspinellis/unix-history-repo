/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include <machine/machAsmDefs.h>

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)modf.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/*
 * double modf(val, iptr)
 *	double val, *iptr;
 * returns: xxx and n (in *iptr) where val == n.xxx
 */
LEAF(modf)
	cfc1	t0, $31			# get the control register
	cfc1	t0, $31			# get the control register
	or	t1, t0, 0x3		# set rounding mode to round to zero
	xor	t1, t1, 0x2		#  (i.e., 01)
	ctc1	t1, $31
	cvt.w.d	$f0, $f12		# convert val to integer
	cvt.d.w	$f0, $f0		# convert back to double
	ctc1	t0, $31			# restore old rounding mode
	s.d	$f0, 0(a2)		# save the integer part
	sub.d	$f0, $f12, $f0		# subtract val - integer part
	j	ra
END(modf)
