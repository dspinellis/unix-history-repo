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
static char sccsid[] = "@(#)fixsfdi.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Convert float to (signed) quad.
 * We clamp anything that is out of range.
 *
 * N.B.: must use new ANSI syntax (sorry).
 */
long long
__fixsfdi(float x)
{
	if (x < 0)
		if (x <= QUAD_MIN)
			return (QUAD_MIN);
		else
			return ((quad_t)-(u_quad_t)-x);
	else
		if (x >= QUAD_MAX)
			return (QUAD_MAX);
		else
			return ((quad_t)(u_quad_t)x);
}
