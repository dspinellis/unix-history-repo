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
static char sccsid[] = "@(#)negdi2.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Return -a (or, equivalently, 0 - a), in quad.  See subdi3.c.
 */
quad_t
__negdi2(a)
	quad_t a;
{
	union uu aa, res;

	aa.q = a;
	res.ul[L] = -aa.ul[L];
	res.ul[H] = -aa.ul[H] - (res.ul[L] > 0);
	return (res.q);
}
