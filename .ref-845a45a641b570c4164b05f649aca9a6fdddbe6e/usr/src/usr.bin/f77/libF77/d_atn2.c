/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_atn2.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_atn2(x,y)
double *x, *y;
{
double atan2();
return( atan2(*x,*y) );
}
