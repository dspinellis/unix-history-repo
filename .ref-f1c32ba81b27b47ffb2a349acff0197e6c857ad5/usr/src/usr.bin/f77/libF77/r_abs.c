/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_abs.c	5.3 (Berkeley) %G%";
#endif /* not lint */

float r_abs(x)
float *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
