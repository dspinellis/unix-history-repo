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
static char sccsid[] = "@(#)umoddi3.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Return remainder after dividing two unsigned quads.
 */
u_quad_t
__umoddi3(a, b)
	u_quad_t a, b;
{
	u_quad_t r;

	(void)__qdivrem(a, b, &r);
	return (r);
}
