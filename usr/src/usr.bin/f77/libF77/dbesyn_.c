/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dbesyn_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double yn();

double dbesyn_(n, x)
long *n; double *x;
{
	return(yn((int)*n, *x));
}
