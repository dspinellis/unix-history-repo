/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_atn2.c	5.3 (Berkeley) %G%";
#endif /* not lint */

float r_atn2(x,y)
float *x, *y;
{
double atan2();
return( atan2(*x,*y) );
}
