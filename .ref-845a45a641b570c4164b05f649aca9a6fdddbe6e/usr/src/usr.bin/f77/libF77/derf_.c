/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)derf_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double derf_(x)
double *x;
{
double erf();

return( erf(*x) );
}
