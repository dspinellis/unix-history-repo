/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)derf_.c	5.1	6/7/85
 */

double derf_(x)
double *x;
{
double erf();

return( erf(*x) );
}
