/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "dumb.h"

/* Does not plot first point -- assumed that it is already plotted */
dda_line(ch, x0, y0, x1, y1)
	char ch;
	int x0, y0;	/* already transformed to screen coords */
	int x1, y1;	/* untransformed */
{
	int length, i;
	double deltaX, deltaY;
	double x, y;
	double floor();
	int abs();

	scale(x1, y1);

	length = abs(x1 - x0);
	if (abs(y1 -y0) > length)
		length = abs(y1 - y0);

	if (length == 0)
		return;

	deltaX = (double) (x1 - x0)/(double) length;
	deltaY = (double) (y1 - y0)/(double) length;

	x = (double) x0 + 0.5;
	y = (double) y0 + 0.5;

	for (i=0; i < length; ++i) {
		x += deltaX;
		y += deltaY;
		x0 = floor(x);
		y0 = floor(y);
		currentx = x0;
		currenty = y0;
		screenmat[currentx][currenty] = ch;
	}
}
