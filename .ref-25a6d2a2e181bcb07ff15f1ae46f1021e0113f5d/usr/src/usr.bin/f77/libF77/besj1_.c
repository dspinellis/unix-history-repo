/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)besj1_.c	5.1	%G%
 */

double j1();

float besj1_(x)
float *x;
{
	return((float)j1((double)*x));
}
