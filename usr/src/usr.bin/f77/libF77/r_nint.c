/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)r_nint.c	5.4 (Berkeley) 4/12/91";
#endif /* not lint */

float flt_retval;

float r_nint(x)
float *x;
{
double floor();

flt_retval = (*x)>=0 ? floor(*x + .5) : -floor(.5 - *x);
return(flt_retval);
}
