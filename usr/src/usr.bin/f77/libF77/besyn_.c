/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)besyn_.c	5.1	%G%
 */

double yn();

float besyn_(n, x)
long *n; float *x;
{
	return((float)yn((int)*n, (double)*x));
}
