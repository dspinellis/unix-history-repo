/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)besjn_.c	5.1	%G%
 */

double jn();

float besjn_(n, x)
long *n; float *x;
{
	return((float)jn((int)*n, (double)*x));
}
