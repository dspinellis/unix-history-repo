/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)box.c	5.2 (Berkeley) 6/8/85";
#endif not lint

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
