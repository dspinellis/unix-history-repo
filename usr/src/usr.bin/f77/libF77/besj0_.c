/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)besj0_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double j0();

float besj0_(x)
float *x;
{
	return((float)j0((double)*x));
}
