/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_sqrt.c	5.1	%G%
 */

double d_sqrt(x)
double *x;
{
double sqrt();
return( sqrt(*x) );
}
