/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_sign.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#ifndef tahoe
double d_sign(a,b)
double *a, *b;
{
double x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}

#else

#include <tahoe/math/FP.h>

double d_sign(a,b)
double *a, *b;
{
double x;
x = *a;
if ((*a < 0) || (*b < 0))
	*(unsigned long *)&x ^= SIGN_BIT;
return(x);
}
#endif
