/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_exp.c	5.1	%G%
 */

double d_exp(x)
double *x;
{
double exp();
return( exp(*x) );
}
