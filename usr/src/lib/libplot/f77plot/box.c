/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)box.c	5.3 (Berkeley) %G%";
#endif /* not lint */

box_(x0, y0, x1, y1)
int *x0, *y0, *x1, *y1;
{
	move(*x0, *y0);
	cont(*x0, *y1);
	cont(*x1, *y1);
	cont(*x1, *y0);
	cont(*x0, *y0);
	move(*x1, *y1);
}
