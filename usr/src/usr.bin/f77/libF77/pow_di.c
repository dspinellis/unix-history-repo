/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pow_di.c	5.4 (Berkeley) %G%";
#endif /* not lint */

double
pow_di(ap, bp)
	double *ap;
	long *bp;
{
	register long n = *bp;
	double y, x = *ap;

	if (!n)
		return((double)1);
	if (n < 0) {
		x = (double)1 / x;
		n = -n;
	}
	while (!(n&1)) {
		x *= x;
		n >>= 1;
	}
	for (y = x; --n > 0; y *= x)
		while (!(n&1)) {
			x *= x;
			n >>= 1;
		}
	return(y);
}
