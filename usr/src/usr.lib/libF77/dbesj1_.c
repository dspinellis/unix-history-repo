/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dbesj1_.c	5.1	6/7/85
 */

double j1();

double dbesj1_(x)
double *x;
{
	return(j1(*x));
}
