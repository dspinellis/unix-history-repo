/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_ri.c	5.4	%G%
 */

#ifdef tahoe
#define	double	float
#endif /* tahoe */

float
pow_ri(ap, bp)
	float *ap;
	long *bp;
{
	register long n = *bp;
#ifdef tahoe
	register
#endif /* tahoe */
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
#ifdef tahoe
#undef double
#endif /* tahoe */
