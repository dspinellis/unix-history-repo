/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)frexp.s	5.1 (Berkeley) %G%";
#endif not lint

/* C library -- frexp(value, eptr) */

#include "DEFS.h"

ENTRY(frexp, 0)
	movd	4(ap),r0		# (r0,r1) := value
	extzv	$7,$8,r0,*12(ap)	# Fetch exponent
	jeql	1f			# If exponent zero, we're done
	subl2	$128,*12(ap)		# Bias the exponent appropriately
	insv	$128,$7,$8,r0		# Force result exponent to biased 0
1:
	ret
