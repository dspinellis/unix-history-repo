/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)besy0_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double y0();

float besy0_(x)
float *x;
{
	return((float)y0((double)*x));
}
