/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)floatunsdidf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Convert (unsigned) quad to double.
 * This is exactly like floatdidf.c except that negatives never occur.
 */
double
__floatunsdidf(x)
	u_quad_t x;
{
	double d;
	union uu u;

	u.uq = x;
	d = (double)u.ul[H] * ((1 << (LONG_BITS - 2)) * 4.0);
	d += u.ul[L];
	return (d);
}
