/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_dim.c	5.4 (Berkeley) %G%";
#endif /* not lint */

float flt_retval;

float r_dim(a,b)
float *a, *b;
{
flt_retval =  *a > *b ? *a - *b : 0;
return(flt_retval);
}
