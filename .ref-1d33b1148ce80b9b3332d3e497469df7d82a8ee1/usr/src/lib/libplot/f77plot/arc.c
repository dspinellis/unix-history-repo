/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)arc.c	8.1 (Berkeley) %G%";
#endif /* not lint */

arc_(x,y,x0,y0,x1,y1)
int *x, *y, *x0, *y0, *x1, *y1;
{
	arc(*x, *y, *x0, *y0, *x1, *y1);
}
