/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)arc.c	5.1 (Berkeley) 6/7/85";
#endif not lint

arc_(x,y,x0,y0,x1,y1)
int *x, *y, *x0, *y0, *x1, *y1;
{
	arc(*x, *y, *x0, *y0, *x1, *y1);
}
