/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_dprod.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_dprod(x,y)
double *x, *y;
{
/* dprod with -r8 flag - all in double precision */
return( (*x) * (*y) );
}
