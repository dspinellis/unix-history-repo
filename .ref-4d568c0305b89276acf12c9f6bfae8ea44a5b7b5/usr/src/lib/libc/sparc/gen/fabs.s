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
 * from: $Header: fabs.s,v 1.4 91/10/07 23:59:05 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fabs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* fabs - floating absolute value */

#include "DEFS.h"

ENTRY(fabs)
	std	%o0, [%sp + 32]		! return value => %f0:f1
	ldd	[%sp + 32], %f0		! (via kernel %o0/%o1 slot)
	retl
	 fabss	%f0, %f0		! return absolute value
