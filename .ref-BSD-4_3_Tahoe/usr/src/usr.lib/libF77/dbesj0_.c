/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dbesj0_.c	5.1	6/7/85
 */

double j0();

double dbesj0_(x)
double *x;
{
	return(j0(*x));
}
