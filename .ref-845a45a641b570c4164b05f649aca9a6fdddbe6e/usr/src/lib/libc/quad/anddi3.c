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
static char sccsid[] = "@(#)anddi3.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Return a & b, in quad.
 */
quad_t
__anddi3(a, b)
	quad_t a, b;
{
	union uu aa, bb;

	aa.q = a;
	bb.q = b;
	aa.ul[0] &= bb.ul[0];
	aa.ul[1] &= bb.ul[1];
	return (aa.q);
}
