/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dbesy0_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double y0();

double dbesy0_(x)
double *x;
{
	return(y0(*x));
}
