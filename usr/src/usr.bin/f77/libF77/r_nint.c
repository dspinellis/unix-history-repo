/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_nint.c	5.4 (Berkeley) %G%";
#endif /* not lint */

float flt_retval;

float r_nint(x)
float *x;
{
double floor();

flt_retval = (*x)>=0 ? floor(*x + .5) : -floor(.5 - *x);
return(flt_retval);
}
