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
static char sccsid[] = "@(#)lshrdi3.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Shift an (unsigned) quad value right (logical shift right).
 */
quad_t
__lshrdi3(a, shift)
	quad_t a;
	qshift_t shift;
{
	union uu aa;

	aa.q = a;
	if (shift >= LONG_BITS) {
		aa.ul[L] = shift >= QUAD_BITS ? 0 :
		    aa.ul[H] >> (shift - LONG_BITS);
		aa.ul[H] = 0;
	} else if (shift > 0) {
		aa.ul[L] = (aa.ul[L] >> shift) |
		    (aa.ul[H] << (LONG_BITS - shift));
		aa.ul[H] >>= shift;
	}
	return (aa.q);
}
