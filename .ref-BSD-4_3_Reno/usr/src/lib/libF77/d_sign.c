/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_sign.c	5.2	11/3/86
 */

#ifndef tahoe
double d_sign(a,b)
double *a, *b;
{
double x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}

#else tahoe

#include <tahoemath/FP.h>

double d_sign(a,b)
double *a, *b;
{
double x;
x = *a;
if ((*a < 0) || (*b < 0))
	*(unsigned long *)&x ^= SIGN_BIT;
return(x);
}
#endif tahoe
