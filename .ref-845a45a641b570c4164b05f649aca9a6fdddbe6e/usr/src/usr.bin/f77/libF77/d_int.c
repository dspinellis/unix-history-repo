/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_int.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_int(x)
double *x;
{
double floor();

return( (*x >= 0) ? floor(*x) : -floor(- *x) );
}
