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
static char sccsid[] = "@(#)iordi3.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Return a | b, in quad.
 */
quad
__iordi3(quad a, quad b)
{
	union uu aa, bb;

	aa.q = a;
	bb.q = b;
	aa.ul[0] |= bb.ul[0];
	aa.ul[1] |= bb.ul[1];
	return (aa.q);
}
