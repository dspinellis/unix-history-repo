/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)besy0_.c	5.1	6/7/85
 */

double y0();

float besy0_(x)
float *x;
{
	return((float)y0((double)*x));
}
