/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)besj1_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double j1();

float besj1_(x)
float *x;
{
	return((float)j1((double)*x));
}
