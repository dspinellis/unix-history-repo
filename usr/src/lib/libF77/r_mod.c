/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_mod.c	5.4	11/3/86
 */

#ifndef tahoe
float flt_retval;

float r_mod(x,y)
float *x, *y;
{
double floor(), quotient = *x / *y;
if (quotient >= 0.0)
	quotient = floor(quotient);
else
	quotient = -floor(-quotient);
flt_retval = *x - (*y) * quotient ;
return(flt_retval);
}

#else tahoe

/*   THIS IS BASED ON THE TAHOE REPR. FOR FLOATING POINT */
#include <tahoemath/FP.h>

double r_mod(x,y)
float *x, *y;
{
double floor(), quotient = *x / *y;
if (quotient >= 0.0)
	quotient = floor(quotient);
else {
	*(unsigned long *)&quotient ^= SIGN_BIT;
	quotient = floor(quotient);
	if (quotient != 0)
		*(unsigned long *)&quotient ^= SIGN_BIT;
	}
return(*x - (*y) * quotient );
}
#endif tahoe
