/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)pow_zi.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

#include "complex"

#define	Z_MULEQ(A,B)	\
	t = (A).dreal * (B).dreal - (A).dimag * (B).dimag,\
	(A).dimag = (A).dreal * (B).dimag + (A).dimag * (B).dreal,\
	(A).dreal = t	/* A *= B */

void
pow_zi(p, a, b) 	/* p = a**b  */
	dcomplex *p, *a;
	long int *b;
{
	register long n = *b;
	double t;
	dcomplex x;

	x = *a;
	p->dreal = (double)1, p->dimag = (double)0;
	if (!n)
		return;
	if (n < 0) {
		z_div(&x, p, a);
		n = -n;
	}
	while (!(n&1)) {
		Z_MULEQ(x, x);
		n >>= 1;
	}
	for (*p = x; --n > 0; Z_MULEQ(*p, x))
		while (!(n&1)) {
			Z_MULEQ(x, x);
			n >>= 1;
		}
}
