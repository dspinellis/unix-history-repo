/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include <machine/machAsmDefs.h>

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)fabs.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

	.set	noreorder

/*
 * fabs(x)
 *	double x;
 *
 * Return absolute value of x.
 */
LEAF(fabs)
	j	ra
	abs.d	$f0, $f12		# compute absolute value of x
END(fabs)
