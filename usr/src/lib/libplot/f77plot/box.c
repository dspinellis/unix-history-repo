/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)box.c	5.1 (Berkeley) %G%";
#endif not lint

box_(x0, y0, x1, y1)
int *x0, *y0, *x1, *y1;
{
	box(*x0, *y0, *x1, *y1);
}
