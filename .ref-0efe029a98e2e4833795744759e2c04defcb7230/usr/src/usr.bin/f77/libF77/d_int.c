/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_int.c	5.1	%G%
 */

double d_int(x)
double *x;
{
double floor();

return( (*x >= 0) ? floor(*x) : -floor(- *x) );
}
