/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dbesjn_.c	5.1	%G%
 */

double jn();

double dbesjn_(n, x)
long *n; double *x;
{
	return(jn((int)*n, *x));
}
