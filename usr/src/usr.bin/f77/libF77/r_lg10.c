/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_lg10.c	5.1	%G%
 */

#define log10e 0.43429448190325182765

double r_lg10(x)
float *x;
{
double log();

return( log10e * log(*x) );
}
