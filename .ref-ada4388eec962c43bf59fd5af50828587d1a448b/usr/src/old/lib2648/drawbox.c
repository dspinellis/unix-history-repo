/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)drawbox.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Draw a box around a window.  The lower left corner of the box is at (r, c).
 * Color is 1 for drawing a box, 0 for erasing.
 * The box is nrow by ncol.
 */

#include "2648.h"

drawbox(r, c, color, nrow, ncol)
int r, c, color, nrow, ncol;
{
	if (color)
		setset();
	else
		setclear();
	move(c, r);
	draw(c+ncol-1, r);
	draw(c+ncol-1, r+nrow-1);
	draw(c, r+nrow-1);
	draw(c, r);
}
