/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_nint.c	5.3	%G%
 */

float flt_retval;

float r_nint(x)
float *x;
{
double floor();

flt_retval = (*x)>=0 ? floor(*x + .5) : -floor(.5 - *x);
return(flt_retval);
}
