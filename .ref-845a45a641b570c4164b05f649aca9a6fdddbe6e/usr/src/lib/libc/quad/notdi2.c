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
static char sccsid[] = "@(#)notdi2.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "quad.h"

/*
 * Return ~a.  For some reason gcc calls this `one's complement' rather
 * than `not'.
 */
quad_t
__one_cmpldi2(a)
	quad_t a;
{
	union uu aa;

	aa.q = a;
	aa.ul[0] = ~aa.ul[0];
	aa.ul[1] = ~aa.ul[1];
	return (aa.q);
}
