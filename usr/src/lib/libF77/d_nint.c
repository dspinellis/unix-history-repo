/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_nint.c	5.1	6/7/85
 */

double d_nint(x)
double *x;
{
double floor();

return( (*x)>=0 ?
	floor(*x + .5) : -floor(.5 - *x) );
}
