/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)besy1_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double y1();

float besy1_(x)
float *x;
{
	return((float)y1((double)*x));
}
