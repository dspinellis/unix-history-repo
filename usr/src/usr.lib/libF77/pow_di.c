/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_di.c	5.3	1/19/88
 */

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
