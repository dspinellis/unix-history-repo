/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_lg10.c	5.3	7/8/85
 */

float r_lg10(x)
float *x;
{
double log10();

return( log10(*x) );
}
