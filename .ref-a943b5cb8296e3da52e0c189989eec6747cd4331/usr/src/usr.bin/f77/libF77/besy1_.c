/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)besy1_.c	5.1	%G%
 */

double y1();

float besy1_(x)
float *x;
{
	return((float)y1((double)*x));
}
