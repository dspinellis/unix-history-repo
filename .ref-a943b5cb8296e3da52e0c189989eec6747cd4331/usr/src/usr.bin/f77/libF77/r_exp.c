/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_exp.c	5.2	%G%
 */

float r_exp(x)
float *x;
{
double exp();
return( exp(*x) );
}
