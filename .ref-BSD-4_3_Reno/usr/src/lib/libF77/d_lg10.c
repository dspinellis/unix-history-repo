/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_lg10.c	5.3	8/8/85
 */

double d_lg10(x)
double *x;
{
double log10();

return( log10(*x) );
}
