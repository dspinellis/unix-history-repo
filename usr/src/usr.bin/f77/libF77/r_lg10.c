/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_lg10.c	5.4 (Berkeley) %G%";
#endif /* not lint */

float r_lg10(x)
float *x;
{
double log10();

return( log10(*x) );
}
