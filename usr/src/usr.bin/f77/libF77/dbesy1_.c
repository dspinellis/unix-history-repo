/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dbesy1_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double y1();

double dbesy1_(x)
double *x;
{
	return(y1(*x));
}
