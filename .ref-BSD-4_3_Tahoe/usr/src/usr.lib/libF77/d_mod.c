/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_mod.c	5.3	5/8/87
 */
#ifdef tahoe
#include <tahoemath/FP.h>
#endif tahoe

double d_mod(x,y)
double *x, *y;
{
double floor(), quotient = *x / *y;
if (quotient >= 0.0)
	quotient = floor(quotient);
else {
#ifndef tahoe
	quotient = -floor(-quotient);
#else tahoe
	*(unsigned long *)&quotient ^= SIGN_BIT;
	quotient = floor(quotient);
	if (quotient !=0)
		*(unsigned long *)&quotient ^= SIGN_BIT;
#endif tahoe
}
return(*x - (*y) * quotient );
}
