/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dbesyn_.c	5.1	6/7/85
 */

double yn();

double dbesyn_(n, x)
long *n; double *x;
{
	return(yn((int)*n, *x));
}
