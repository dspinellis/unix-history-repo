/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_cosh.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_cosh(x)
double *x;
{
double cosh();
return( cosh(*x) );
}
