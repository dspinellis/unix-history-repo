/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fixdfdi.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

#ifndef QUAD_MAX	/* should be in <limits.h> maybe? */
#define	QUAD_MAX ((quad)(((u_quad)1 << (QUAD_BITS - 1)) - 1))
#define	QUAD_MIN (-QUAD_MAX - 1)
#endif

/*
 * Convert double to (signed) quad.
 * We clamp anything that is out of range.
 */
quad
__fixdfdi(double x)
{
	if (x < 0)
		if (x <= QUAD_MIN)
			return (QUAD_MIN);
		else
			return ((quad)-(u_quad)-x);
	else
		if (x >= QUAD_MAX)
			return (QUAD_MAX);
		else
			return ((quad)(u_quad)x);
}
