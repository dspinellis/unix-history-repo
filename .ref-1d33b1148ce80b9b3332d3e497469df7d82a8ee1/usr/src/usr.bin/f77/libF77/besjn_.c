/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)besjn_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double jn();

float besjn_(n, x)
long *n; float *x;
{
	return((float)jn((int)*n, (double)*x));
}
