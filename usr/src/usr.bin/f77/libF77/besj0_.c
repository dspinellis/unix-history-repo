/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)besj0_.c	5.1	%G%
 */

double j0();

float besj0_(x)
float *x;
{
	return((float)j0((double)*x));
}
