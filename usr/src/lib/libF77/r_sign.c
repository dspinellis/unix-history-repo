/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_sign.c	5.2	7/8/85
 */

float r_sign(a,b)
float *a, *b;
{
float x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}
