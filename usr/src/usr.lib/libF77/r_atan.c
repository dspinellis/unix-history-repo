/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_atan.c	5.2	7/8/85
 */

float r_atan(x)
float *x;
{
double atan();
return( atan(*x) );
}
