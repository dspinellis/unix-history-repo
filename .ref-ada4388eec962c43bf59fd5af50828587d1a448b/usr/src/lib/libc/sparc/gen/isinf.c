/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: isinf.c,v 1.1 91/07/08 19:03:34 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)isinf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <machine/ieee.h>

isinf(d)
	double d;
{
	register struct ieee_double *p = (struct ieee_double *)&d;

	return (p->dbl_exp == DBL_EXP_INFNAN &&
	    p->dbl_frach == 0 && p->dbl_fracl == 0);
}
