/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erfc_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

float erfc_(x)
float *x;
{
double erfc();

return( erfc(*x) );
}
