/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)i_abs.c	5.2 (Berkeley) %G%";
#endif /* not lint */

long int i_abs(x)
long int *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
