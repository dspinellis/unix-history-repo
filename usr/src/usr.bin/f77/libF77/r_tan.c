/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_tan.c	5.1	%G%
 */

double r_tan(x)
float *x;
{
double tan();
return( tan(*x) );
}
