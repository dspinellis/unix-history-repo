/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_abs.c	5.2 (Berkeley) %G%";
#endif /* not lint */

double d_abs(x)
double *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
